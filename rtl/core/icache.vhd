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

----------------------------------------------------------------------------------------------------
-- This is a very simple direct mapped cache with 4 bytes cache lines (i.e. only one instruction
-- word per cache line).
--
-- Cache hit:
--   1) Incoming request -> read data + tag
--   2) Tag matches -> respond with data
--
-- Cache miss:
--   1) Incoming request -> read data + tag
--   2) Tag does not match -> start memory request
--   3) (optional) ACK is low -> continue waiting
--   4) ACK is high -> Write data + tag to cache, respond with data from memory
----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.config.all;

entity icache is
  generic(
    LOG2_NUM_LINES : integer := 10;  -- 1024 cache lines
    CONFIG : T_CORE_CONFIG
  );
  port(
    -- Control signals.
    i_clk : in std_logic;
    i_rst : in std_logic;

    i_invalidate : in std_logic;  -- Invalidate the cache.

    -- Instruction interface (WB slave).
    i_instr_cyc : in std_logic;
    i_instr_stb : in std_logic;
    i_instr_adr : in std_logic_vector(C_WORD_SIZE-1 downto 2);
    o_instr_dat : out std_logic_vector(C_WORD_SIZE-1 downto 0);
    o_instr_ack : out std_logic;
    o_instr_stall : out std_logic;
    o_instr_err : out std_logic;

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
  constant C_NUM_LINES : integer := 2 ** LOG2_NUM_LINES;

  -- A tag is encoded as follows: [VAAA...], where:
  --  V      = Valid (1 = valid, 0 = undefined)
  --  AAA... = Address (most significant bits of the memory address)
  constant C_TAG_SIZE : integer := C_WORD_SIZE - 2 - LOG2_NUM_LINES + 1;

  -- Cache read signals.
  signal s_read_addr : std_logic_vector(LOG2_NUM_LINES-1 downto 0);
  signal s_read_tag : std_logic_vector(C_TAG_SIZE-1 downto 0);
  signal s_read_data : std_logic_vector(C_WORD_SIZE-1 downto 0);

  -- Cache write signals.
  signal s_we : std_logic;
  signal s_write_addr : std_logic_vector(LOG2_NUM_LINES-1 downto 0);
  signal s_write_tag : std_logic_vector(C_TAG_SIZE-1 downto 0);
  signal s_write_data : std_logic_vector(C_WORD_SIZE-1 downto 0);

  -- Signals for the cache invalidation state machine.
  signal s_is_invalidating : std_logic;
  signal s_invalidate_idx : integer range 0 to C_NUM_LINES-1;

  -- Signals for the cache lookup & memory request logic.
  signal s_lookup_active : std_logic;
  signal s_lookup_addr : std_logic_vector(C_WORD_SIZE-1 downto 2);
  signal s_lookup_hit : std_logic;
  signal s_memreq_active : std_logic;
  signal s_memreq_addr : std_logic_vector(C_WORD_SIZE-1 downto 2);

  function make_tag(addr : std_logic_vector(C_WORD_SIZE-1 downto 2); is_valid : std_logic) return std_logic_vector is
    variable v_tag : std_logic_vector(C_TAG_SIZE-1 downto 0);
  begin
    v_tag(C_TAG_SIZE-1) := is_valid;
    v_tag(C_TAG_SIZE-2 downto 0) := addr(C_WORD_SIZE-1 downto LOG2_NUM_LINES+2);
    return v_tag;
  end function;
begin
  ICACHE_GEN: if LOG2_NUM_LINES > 0 generate
  begin
    -- Instantiate the tag RAM.
    tag_ram_0: entity work.ram_dual_port
      generic map (
        WIDTH => C_TAG_SIZE,
        ADDR_BITS => LOG2_NUM_LINES
      )
      port map (
        i_clk => i_clk,
        i_write_addr => s_write_addr,
        i_write_data => s_write_tag,
        i_we => s_we,
        i_read_addr => s_read_addr,
        o_read_data => s_read_tag
      );

    -- Instantiate the data RAM.
    data_ram_0: entity work.ram_dual_port
      generic map (
        WIDTH => C_WORD_SIZE,
        ADDR_BITS => LOG2_NUM_LINES
      )
      port map (
        i_clk => i_clk,
        i_write_addr => s_write_addr,
        i_write_data => s_write_data,
        i_we => s_we,
        i_read_addr => s_read_addr,
        o_read_data => s_read_data
      );

    process(i_rst, i_clk)
    begin
      if i_rst = '1' then
        s_is_invalidating <= '1';
        s_invalidate_idx <= 0;
        s_lookup_active <= '0';
        s_lookup_addr <= (others => '0');
        s_memreq_active <= '0';
        s_memreq_addr <= (others => '0');
      else
        if s_is_invalidating = '1' then
          -- We are invalidating the cache.
          if s_invalidate_idx = C_NUM_LINES-1 then
            s_is_invalidating <= '0';
          end if;
          s_invalidate_idx <= s_invalidate_idx + 1;
        else
          if i_invalidate = '1' then
            -- We should start invalidating the cache.
            s_is_invalidating <= '1';
            s_invalidate_idx <= 0;
          else
            -- Initiate a new cache lookup?
            s_lookup_active <= i_instr_cyc and i_instr_stb;
            s_lookup_addr <= i_instr_adr;

            -- Initiate and end memory requests.
            if s_memreq_active = '1' and i_mem_ack = '0' then
              o_mem_stb <= '0';
            elsif s_lookup_active = '1' and s_lookup_hit = '0' then
              s_memreq_active <= '1';
              s_memreq_addr <= s_lookup_addr;
              o_mem_adr <= s_lookup_addr;
              o_mem_stb <= '1';
              o_mem_cyc <= '1';
            else
              s_memreq_active <= '0';
              o_mem_stb <= '0';
              o_mem_cyc <= '0';
            end if;
          end if;
        end if;
      end if;
    end process;

    ------------------------------------------------------------------------------------------------
    -- Cache read logic.
    ------------------------------------------------------------------------------------------------

    s_read_addr <= i_instr_adr(LOG2_NUM_LINES+1 downto 2);

    s_lookup_hit <= '1' when s_read_tag = make_tag(s_lookup_addr, '1') else '0';

    ------------------------------------------------------------------------------------------------
    -- Cache write logic.
    ------------------------------------------------------------------------------------------------

    s_we <= s_is_invalidating or (s_memreq_active and i_mem_ack);

    s_write_addr <=
        std_logic_vector(to_unsigned(s_invalidate_idx, LOG2_NUM_LINES)) when s_is_invalidating = '1' else
        s_memreq_addr(LOG2_NUM_LINES+1 downto 2);

    s_write_tag <=
        make_tag(30x"0", '0') when s_is_invalidating = '1' else
        make_tag(s_memreq_addr, '1');

    s_write_data <=
        32x"0" when s_is_invalidating = '1' else
        i_mem_dat;

    ------------------------------------------------------------------------------------------------
    -- Cache response logic.
    ------------------------------------------------------------------------------------------------

    o_instr_dat <= s_read_data;
    o_instr_ack <= s_lookup_active and s_lookup_hit;
    o_instr_stall <= s_lookup_active and not s_lookup_hit;
    o_instr_err <= '0';

  else generate
    -- We just forward all requests to the main memory interface.
    o_mem_cyc <= i_instr_cyc;
    o_mem_stb <= i_instr_stb;
    o_mem_adr <= i_instr_adr;

    -- ...and send the result right back.
    o_instr_dat <= i_mem_dat;
    o_instr_ack <= i_mem_ack;
    o_instr_stall <= i_mem_stall;
    o_instr_err <= i_mem_err;
  end generate;
end rtl;
