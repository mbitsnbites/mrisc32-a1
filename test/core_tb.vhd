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
library std;
use std.textio.all;
use work.types.all;
use work.config.all;
use work.debug.all;

entity core_tb is
end core_tb;

architecture behavioral of core_tb is
  signal s_clk : std_logic;
  signal s_rst : std_logic;

  -- Instr-to-mem interface.
  signal s_instr_cyc : std_logic;
  signal s_instr_stb : std_logic;
  signal s_instr_adr : std_logic_vector(C_WORD_SIZE-1 downto 2);

  -- Mem-to-instr interface.
  signal s_instr_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_instr_ack : std_logic;
  signal s_instr_stall : std_logic;
  signal s_instr_err : std_logic;

  -- Data-to-mem interface.
  signal s_data_cyc : std_logic;
  signal s_data_stb : std_logic;
  signal s_data_adr : std_logic_vector(C_WORD_SIZE-1 downto 2);
  signal s_data_dat_w : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_data_we : std_logic;
  signal s_data_sel : std_logic_vector(C_WORD_SIZE/8-1 downto 0);

  -- Mem-to-data interface.
  signal s_data_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);
  signal s_data_ack : std_logic;
  signal s_data_stall : std_logic;
  signal s_data_err : std_logic;

  -- Debug trace interface.
  signal s_debug_trace : T_DEBUG_TRACE;
begin
  core_0: entity work.core
    port map (
      i_clk => s_clk,
      i_rst => s_rst,

      -- Instruction interface.
      o_imem_cyc => s_instr_cyc,
      o_imem_stb => s_instr_stb,
      o_imem_adr => s_instr_adr,
      i_imem_dat => s_instr_dat,
      i_imem_ack => s_instr_ack,
      i_imem_stall => s_instr_stall,
      i_imem_err => s_instr_err,

      -- Data interface.
      o_dmem_cyc => s_data_cyc,
      o_dmem_stb => s_data_stb,
      o_dmem_adr => s_data_adr,
      o_dmem_dat => s_data_dat_w,
      o_dmem_we => s_data_we,
      o_dmem_sel => s_data_sel,
      i_dmem_dat => s_data_dat,
      i_dmem_ack => s_data_ack,
      i_dmem_stall => s_data_stall,
      i_dmem_err => s_data_err,

      -- Debug trace interface.
      o_debug_trace => s_debug_trace
    );

  process
    -- We have a memory array that represents 512KB or RAM (for program and data).
    constant C_MEM_NUM_WORDS : integer := 2**17;
    type T_MEM_ARRAY is array (0 to C_MEM_NUM_WORDS-1) of std_logic_vector(C_WORD_SIZE-1 downto 0);
    variable v_mem_array : T_MEM_ARRAY;

    -- File I/O.
    type T_CHAR_FILE is file of character;
    file f_char_file : T_CHAR_FILE;
    file f_trace_file : T_CHAR_FILE;

    -- Variables for the memory interface.
    variable v_mem_idx : integer;
    variable v_instr_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);
    variable v_instr_ack : std_logic;
    variable v_write_mask : std_logic_vector(C_WORD_SIZE-1 downto 0);
    variable v_data_dat : std_logic_vector(C_WORD_SIZE-1 downto 0);
    variable v_data_ack : std_logic;

    -- How many CPU cycles should we simulate?
    constant C_TEST_CYCLES : integer := 20000000;

    -- Helper function for reading one word from a binary file.
    function read_word(file f : T_CHAR_FILE) return std_logic_vector is
      variable v_char : character;
      variable v_byte : std_logic_vector(7 downto 0);
      variable v_word : std_logic_vector(C_WORD_SIZE-1 downto 0);
    begin
      for i in 0 to (C_WORD_SIZE/8)-1 loop
        read(f, v_char);
        v_byte := std_logic_vector(to_unsigned(character'pos(v_char), 8));
        v_word(((i+1)*8)-1 downto i*8) := v_byte;
      end loop;
      return v_word;
    end function;

    -- Helper function for writing one word to a binary file.
    procedure write_word(file f : T_CHAR_FILE; word : std_logic_vector(C_WORD_SIZE-1 downto 0)) is
      variable v_char : character;
      variable v_byte : std_logic_vector(7 downto 0);
    begin
      for i in 0 to (C_WORD_SIZE/8)-1 loop
        v_byte := word(((i+1)*8)-1 downto i*8);
        v_char := character'val(to_integer(unsigned(v_byte)));
        write(f, v_char);
      end loop;
    end procedure;

    -- Helper function for writing a debug trace record to a binary file.
    procedure write_trace(file f : T_CHAR_FILE; trace : T_DEBUG_TRACE) is
      variable v_flags : std_logic_vector(C_WORD_SIZE-1 downto 0);
    begin
      v_flags := (0 => trace.valid,
                  1 => trace.src_a_valid,
                  2 => trace.src_b_valid,
                  3 => trace.src_c_valid,
                  others => '0');
      write_word(f, v_flags);
      write_word(f, trace.pc);
      if trace.src_a_valid then
        write_word(f, trace.src_a);
      else
        write_word(f, (others => '0'));
      end if;
      if trace.src_b_valid then
        write_word(f, trace.src_b);
      else
        write_word(f, (others => '0'));
      end if;
      if trace.src_c_valid then
        write_word(f, trace.src_c);
      else
        write_word(f, (others => '0'));
      end if;
    end procedure;
  begin
    -- Clear the memory with zeros.
    for i in 0 to C_MEM_NUM_WORDS-1 loop
      v_mem_array(i) := to_word(0);
    end loop;

    -- Read the program to run from the binary file core_tb_prg.bin.
    file_open(f_char_file, "out/core_tb_prg.bin");
    -- v_mem_idx := to_integer(unsigned(read_word(f_char_file)))/4;  -- Fist word = program start.
    v_mem_idx := 128;  -- Starting address is 0x00000200 (/4)
    while not endfile(f_char_file) loop
      v_mem_array(v_mem_idx) := read_word(f_char_file);
      v_mem_idx := v_mem_idx + 1;
    end loop;
    file_close(f_char_file);

    -- Open the debug trace file.
    if C_DEBUG_ENABLE_TRACE then
      file_open(f_trace_file, "/tmp/mrisc32_core_tb_trace.bin", WRITE_MODE);
    end if;

    -- Reset the memory-to-core signals.
    s_instr_dat <= (others => '0');
    s_instr_ack <= '0';
    s_instr_stall <= '0';
    s_instr_err <= '0';
    s_data_dat <= (others => '0');
    s_data_ack <= '0';
    s_data_stall <= '0';
    s_data_err <= '0';

    -- Start by resetting the core (to have defined signals).
    s_rst <= '1';
    s_clk <= '1';
    wait for 5 ns;
    s_clk <= '0';
    wait for 5 ns;
    s_clk <= '1';
    wait for 5 ns;
    s_rst <= '0';
    s_clk <= '0';
    wait for 5 ns;
    s_clk <= '1';
    wait for 5 ns;
    s_clk <= '0';
    wait for 5 ns;

    -- Run the program.
    for i in 2 to C_TEST_CYCLES-1 loop
      -- Print progress.
      if (i mod 10000) = 0 then
        report "Cycles: " & integer'image(i);
      end if;

      -- We should now have memory requests from the Wishbone interfaces.

      -- Instruction read from the memory.
      v_instr_dat := X"00000000";
      if s_instr_cyc = '1' and s_instr_stb = '1' then
        v_mem_idx := to_integer(unsigned(s_instr_adr));
        if v_mem_idx = 0 then
          report "Simulation finished after " & integer'image(i) & " cycles.";
          exit;
        elsif (v_mem_idx > 0) and (v_mem_idx < C_MEM_NUM_WORDS) then
          v_instr_dat := v_mem_array(v_mem_idx);
        end if;
        v_instr_ack := '1';
      else
        v_instr_ack := '0';
      end if;

      -- Data read/write from/to the memory.
      v_write_mask(31 downto 24) := (others => s_data_sel(3));
      v_write_mask(23 downto 16) := (others => s_data_sel(2));
      v_write_mask(15 downto 8) := (others => s_data_sel(1));
      v_write_mask(7 downto 0) := (others => s_data_sel(0));
      v_data_dat := X"00000000";
      if s_data_cyc = '1' and s_data_stb = '1' then
        v_mem_idx := to_integer(unsigned(s_data_adr));
        if (v_mem_idx >= 0) and (v_mem_idx < C_MEM_NUM_WORDS) then
          v_data_dat := v_mem_array(v_mem_idx);
        end if;
        if s_data_we = '1' then
          v_data_dat := (v_data_dat and (not v_write_mask)) or (s_data_dat_w and v_write_mask);
          if (v_mem_idx >= 0) and (v_mem_idx < C_MEM_NUM_WORDS) then
            v_mem_array(v_mem_idx) := v_data_dat;
          end if;
        end if;
        v_data_ack := '1';
      else
        v_data_ack := '0';
      end if;

      -- Write a recrod to the debug trace file.
      -- Note: We skip the first few cycles until we are properly reset.
      if C_DEBUG_ENABLE_TRACE and i >= 5 then
        write_trace(f_trace_file, s_debug_trace);
      end if;

      -- Positive clock flank -> time for us to respond on the Wishbone requests.
      s_clk <= '1';
      wait for 1 ns;

      s_instr_dat <= v_instr_dat;
      s_instr_ack <= v_instr_ack;
      s_data_dat <= v_data_dat;
      s_data_ack <= v_data_ack;

      -- Tick the clock.
      wait for 4 ns;
      s_clk <= '0';
      wait for 5 ns;
    end loop;

    -- Close the debug trace file.
    if C_DEBUG_ENABLE_TRACE then
      file_close(f_trace_file);
    end if;

    -- Dump the memory to a binary file.
    file_open(f_char_file, "/tmp/mrisc32_core_tb_ram.bin", WRITE_MODE);
    for i in 0 to C_MEM_NUM_WORDS-1 loop
      write_word(f_char_file, v_mem_array(i));
    end loop;
    file_close(f_char_file);

    --  Wait forever; this will finish the simulation.
    assert false report "End of test" severity note;
    wait;
  end process;
end behavioral;
