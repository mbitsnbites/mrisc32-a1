----------------------------------------------------------------------------------------------------
-- Copyright (c) 2021 Marcus Geelnard
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

--  A testbench has no ports.
entity icache_tb is
end icache_tb;

architecture behav of icache_tb is
  signal s_clk : std_logic;
  signal s_rst : std_logic;
  signal s_invalidate : std_logic;

  -- Instruction interface (WB slave).
  signal s_instr_cyc : std_logic;
  signal s_instr_stb : std_logic;
  signal s_instr_adr : std_logic_vector(C_WORD_SIZE-1 downto 2);
  signal s_instr_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_instr_ack : std_logic;
  signal s_instr_stall : std_logic;

  -- Memory interface (WB master).
  signal s_mem_cyc : std_logic;
  signal s_mem_stb : std_logic;
  signal s_mem_adr : std_logic_vector(C_WORD_SIZE-1 downto 2);
  signal s_mem_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_mem_ack : std_logic;
  signal s_mem_stall : std_logic;
  signal s_mem_err : std_logic;
begin
  --  Component instantiation.
  icache_0: entity work.icache
    generic map (
      CONFIG => C_CORE_CONFIG_DEFAULT
    )
    port map (
      i_clk => s_clk,
      i_rst => s_rst,
      i_invalidate => s_invalidate,

      i_instr_cyc => s_instr_cyc,
      i_instr_stb => s_instr_stb,
      i_instr_adr => s_instr_adr,
      o_instr_dat => s_instr_dat,
      o_instr_ack => s_instr_ack,
      o_instr_stall => s_instr_stall,

      o_mem_cyc => s_mem_cyc,
      o_mem_stb => s_mem_stb,
      o_mem_adr => s_mem_adr,
      i_mem_dat => s_mem_dat,
      i_mem_ack => s_mem_ack,
      i_mem_stall => s_mem_stall,
      i_mem_err => s_mem_err
    );

  process
    -- The patterns to apply.
    type pattern_type is record
      -- Inputs.
      cyc : std_logic;
      stb : std_logic;
      adr : std_logic_vector(C_WORD_SIZE-1 downto 2);
    end record;
    type pattern_array is array (natural range <>) of pattern_type;
    constant patterns : pattern_array := (
        ('1', '1', 30X"0000123"),
        ('0', '0', 30X"0000000"),
        ('1', '1', 30X"0000123"),
        ('1', '1', 30X"0000124"),
        ('1', '1', 30X"0000125"),
        ('1', '1', 30X"0000123"),
        ('1', '1', 30X"0000122"),
        ('1', '1', 30X"0000125"),
        ('1', '1', 30X"0000124"),
        ('1', '1', 30X"0000123")
      );

    constant C_ADDR_BITS : integer := 5;
    type ROM_ARRAY_T is array (0 to (2**C_ADDR_BITS)-1) of std_logic_vector(31 downto 0);
    constant C_ROM : ROM_ARRAY_T := (
        X"00000001", X"00000002", X"00000003", X"00000004",
        X"00000005", X"00000006", X"00000007", X"00000008",
        X"10000001", X"70000001", X"00010001", X"00070001",
        X"20000001", X"80000001", X"00020001", X"00080001",
        X"30000001", X"90000001", X"00030001", X"00090001",
        X"40000001", X"a0000001", X"00040001", X"000a0001",
        X"50000001", X"b0000001", X"00050001", X"000b0001",
        X"60000001", X"c0000001", X"00060001", X"000c0001"
      );

    constant C_MAX_CYCLES : natural := 2000;

    variable i : natural;
    variable v_num_cycles : natural;
  begin
    -- Reset all inputs.
    s_invalidate <= '0';
    s_instr_cyc <= '0';
    s_instr_stb <= '0';
    s_instr_adr <= (others => '0');
    s_mem_dat <= (others => '0');
    s_mem_ack <= '0';
    s_mem_stall <= '0';
    s_mem_err <= '0';

    -- Reset the entity.
    s_clk <= '0';
    s_rst <= '1';
    wait for 1 ns;
    s_clk <= '1';
    wait for 1 ns;
    s_clk <= '0';
    s_rst <= '0';
    wait for 1 ns;
    s_clk <= '1';
    wait until s_clk = '1';

    -- Test all the patterns in the pattern array.
    i := patterns'left;
    v_num_cycles := 0;
    while i <= patterns'right and v_num_cycles < C_MAX_CYCLES loop
      -- Set the inputs.
      s_instr_cyc <= patterns(i).cyc;
      s_instr_stb <= patterns(i).stb;
      s_instr_adr <= patterns(i).adr;

      -- Tick.
      wait for 1 ns;
      s_clk <= '0';
      wait for 1 ns;
      s_clk <= '1';
      wait until s_clk = '1';

      -- Simulate memory.
      -- TODO(m): Make this more interesting (pipelined / delayed etc).
      if s_mem_stb = '1' then
        s_mem_dat <= C_ROM(to_integer(unsigned(s_mem_adr(C_ADDR_BITS+1 downto 2))));
        s_mem_ack <= '1';
      else
        s_mem_dat <= X"--------";
        s_mem_ack <= '0';
      end if;

      -- Advance?
      if s_instr_ack = '1' then
        i := i + 1;
      end if;

      v_num_cycles := v_num_cycles + 1;
    end loop;
    assert false report "End of test" severity note;
    --  Wait forever; this will finish the simulation.
    wait;
  end process;
end behav;
