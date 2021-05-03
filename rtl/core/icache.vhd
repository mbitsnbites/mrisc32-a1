----------------------------------------------------------------------------------------------------
-- Copyright (c) 2018 Marcus Geelnard
--
-- This software is provided 'as-is', without any express or implied warranty. In no event will the
-- authors be held liable for any damages arising from the use of this software.
--
-- Permission is granted to anyone to use this software for any purpose, including commercial
-- applications, and to alter it and redistribute it freely, subject to the following restrictions:
--
--  1. The origin of this software must not be misrepresented; you must not claim that you wrote
--     the original software. If you use this software in a product, an acknowledgment in the
--     product documentation would be appreciated but is not required.
--
--  2. Altered source versions must be plainly marked as such, and must not be misrepresented as
--     being the original software.
--
--  3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.config.all;
use work.types.all;

entity icache is
  generic(
    CONFIG : T_CORE_CONFIG
  );
  port(
    -- Control signals.
    i_clk : in std_logic;
    i_rst : in std_logic;
    i_invalidate : in std_logic;

    -- Instruction interface (simplified WB slave).
    i_instr_cyc : in std_logic;
    i_instr_stb : in std_logic;
    i_instr_adr : in std_logic_vector(C_WORD_SIZE-1 downto 2);
    o_instr_dat : out std_logic_vector(C_WORD_SIZE-1 downto 0);
    o_instr_ack : out std_logic;
    o_instr_stall : out std_logic;

    -- Memory interface (WB master).
    o_mem_cyc : out std_logic;
    o_mem_stb : out std_logic;
    o_mem_adr : out std_logic_vector(C_WORD_SIZE-1 downto 2);
    i_mem_dat : in std_logic_vector(C_WORD_SIZE-1 downto 0);
    i_mem_ack : in std_logic;
    i_mem_stall : in std_logic;
    i_mem_err : in std_logic
  );
end icache;

architecture rtl of icache is
  type T_STATE is (INVALIDATE, IDLE, CHECK_HIT, WAIT_RESPONSE);

  constant C_LOG2_ENTRIES : natural := 8;
  constant C_TAG_ADDR_SIZE : natural := C_WORD_SIZE-2 - C_LOG2_ENTRIES;
  constant C_TAG_SIZE : natural := C_TAG_ADDR_SIZE + 1;

  signal s_we : std_logic;
  signal s_wr_addr : std_logic_vector(C_LOG2_ENTRIES-1 downto 0);
  signal s_wr_tag : std_logic_vector(C_TAG_SIZE-1 downto 0);
  signal s_wr_data : std_logic_vector(C_WORD_SIZE-1 downto 0);

  signal s_rd_addr : std_logic_vector(C_LOG2_ENTRIES-1 downto 0);
  signal s_rd_tag : std_logic_vector(C_TAG_SIZE-1 downto 0);
  signal s_rd_data : std_logic_vector(C_WORD_SIZE-1 downto 0);

  signal s_state : T_STATE;
  signal s_busy : std_logic;
  signal s_invalidate_requested : std_logic;

  signal s_req : std_logic;
  signal s_response_addr : std_logic_vector(C_WORD_SIZE-1 downto 2);
  signal s_invalidate_addr : unsigned(C_LOG2_ENTRIES-1 downto 0);
  signal s_got_hit : std_logic;

  signal s_mem_start_req : std_logic;
  signal s_mem_pending_ack : std_logic;
  signal s_mem_retry_req : std_logic;

  function make_invalid_tag return std_logic_vector is
  begin
    return '0' & to_vector(0, C_TAG_ADDR_SIZE);
  end function;

  function make_valid_tag(addr : std_logic_vector(C_WORD_SIZE-1 downto 2)) return std_logic_vector is
  begin
    return '1' & addr(C_WORD_SIZE-1 downto C_LOG2_ENTRIES+2);
  end function;
begin
  -- Instantiate the tag RAM.
  tag_ram_0: entity work.ram_dual_port
    generic map (
      WIDTH => C_TAG_SIZE,
      ADDR_BITS => C_LOG2_ENTRIES
    )
    port map (
      i_clk => i_clk,
      i_write_addr => s_wr_addr,
      i_write_data => s_wr_tag,
      i_we => s_we,
      i_read_addr => s_rd_addr,
      o_read_data => s_rd_tag
    );

  -- Instantiate the data RAM.
  data_ram_0: entity work.ram_dual_port
    generic map (
      WIDTH => C_WORD_SIZE,
      ADDR_BITS => C_LOG2_ENTRIES
    )
    port map (
      i_clk => i_clk,
      i_write_addr => s_wr_addr,
      i_write_data => s_wr_data,
      i_we => s_we,
      i_read_addr => s_rd_addr,
      o_read_data => s_rd_data
    );

  -- Lookup logic.
  s_req <= i_instr_cyc and i_instr_stb;
  s_rd_addr <= i_instr_adr(C_LOG2_ENTRIES+1 downto 2);

  -- Cache write logic.
  s_we <= '1' when s_state = INVALIDATE else i_mem_ack;
  s_wr_addr <= std_logic_vector(s_invalidate_addr) when s_state = INVALIDATE else s_response_addr(C_LOG2_ENTRIES+1 downto 2);
  s_wr_data <= i_mem_dat;
  s_wr_tag <= make_invalid_tag when s_state = INVALIDATE else make_valid_tag(s_response_addr);

  -- Cache hit logic.
  s_got_hit <= '1' when s_rd_tag = make_valid_tag(s_response_addr) else '0';

  -- TODO(m): Generate a "hit" if we have a new request for the value that is currently being
  -- read from the memory (before it has been written into the cache). With a 4-byte cache
  -- line this is highly unlikely (the same instruction PC being read multiple times in a row)
  -- but with a larger cache line it's very likely (e.g. reading PC+4 after reading PC).

  -- State machine.
  process (i_rst, i_clk)
  begin
    if i_rst = '1' then
      s_state <= INVALIDATE;
      s_busy <= '1';
      s_invalidate_requested <= '0';
      s_invalidate_addr <= (others => '0');
      s_response_addr <= (others => '0');
    elsif rising_edge(i_clk) then
      case s_state is
        when INVALIDATE =>
          if s_invalidate_addr = unsigned(to_signed(-1, C_LOG2_ENTRIES)) then
            s_state <= IDLE;
            s_busy <= '0';
          end if;
          s_invalidate_addr <= s_invalidate_addr + 1;
          s_invalidate_requested <= '0';

        when IDLE =>
          if i_invalidate = '1' then
            s_state <= INVALIDATE;
            s_invalidate_addr <= (others => '0');
            s_busy <= '1';
          elsif s_req = '1' then
            s_response_addr <= i_instr_adr;
            s_state <= CHECK_HIT;
          end if;

        when CHECK_HIT =>
          if s_got_hit = '1' then
            if i_invalidate = '1' then
              s_state <= INVALIDATE;
              s_invalidate_addr <= (others => '0');
              s_busy <= '1';
            elsif s_req = '1' then
              s_response_addr <= i_instr_adr;
              s_state <= CHECK_HIT;
            else
              s_state <= IDLE;
            end if;
          else
            s_state <= WAIT_RESPONSE;
            s_invalidate_requested <= i_invalidate;
          end if;

        when WAIT_RESPONSE =>
          if i_mem_ack = '1' then
            if i_invalidate = '1' or s_invalidate_requested = '1' then
              s_state <= INVALIDATE;
              s_invalidate_addr <= (others => '0');
              s_busy <= '1';
            else
              if s_req = '1' then
                s_response_addr <= i_instr_adr;
                s_state <= CHECK_HIT;
              else
                s_state <= IDLE;
              end if;
            end if;
          elsif i_invalidate = '1' then
            s_invalidate_requested <= '1';
          end if;

        when others =>
          s_state <= IDLE;
          s_busy <= '0';
      end case;
    end if;
  end process;

  -- Memory interface logic.
  s_mem_start_req <= '1' when s_state = CHECK_HIT and s_got_hit = '0' else '0';
  o_mem_cyc <= s_mem_start_req or s_mem_pending_ack;
  o_mem_stb <= s_mem_start_req or s_mem_retry_req;
  o_mem_adr <= s_response_addr;

  process (i_rst, i_clk)
  begin
    if i_rst = '1' then
      s_mem_pending_ack <= '0';
      s_mem_retry_req <= '0';
    elsif rising_edge(i_clk) then
      -- Keep track of whether or not we're waiting for an ACK (i.e. have an active memory read cycle).
      if s_mem_start_req = '1' then
        s_mem_pending_ack <= '1';
      elsif i_mem_ack = '1' then
        s_mem_pending_ack <= '0';
      end if;

      -- Keep track of whether or not we need to retry the STB (i.e. if the memory is stalling).
      if s_mem_start_req = '1' and i_mem_stall = '1' then
        s_mem_retry_req <= '1';
      elsif i_mem_stall = '0' then
        s_mem_retry_req <= '0';
      end if;
    end if;
  end process;

  -- Host interface logic.
  o_instr_dat <= i_mem_dat when i_mem_ack = '1' else s_rd_data;
  o_instr_ack <= '1' when s_state = CHECK_HIT and s_got_hit = '1' else i_mem_ack;
  o_instr_stall <= '1' when s_busy = '1' or
                            (s_state = CHECK_HIT and s_got_hit = '0') or
                            (s_state = WAIT_RESPONSE and i_mem_ack = '0') else '0';
  -- TODO(m): Handle i_mem_err.
end rtl;
