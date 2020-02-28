LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY Fsm1 IS
PORT(clock: IN STD_LOGIC;
  reset: IN STD_LOGIC;
  input: IN STD_LOGIC;
  y: OUT STD_LOGIC);
END Fsm1;

ARCHITECTURE rtl OF Fsm1 IS
  TYPE StateType IS (s0,sL0,sL1,sY1,sY2);
  SIGNAL state: StateType := s0;
  CONSTANT counter_c_max : INTEGER := 2 ** 22;
  SIGNAL counter_c : UNSIGNED (22 downto 0) := (others => '0');
BEGIN
  PROCESS (clock,reset)
  BEGIN
    IF (reset = '1') THEN
      state <= s0;
    ELSIF rising_edge(clock) THEN
      counter_c <= counter_c + 1;
      CASE state IS
      WHEN s0 =>
        IF input = '0' THEN
          counter_c <= (others => '0');
          state <= sL0;
        END IF;
      WHEN sL0 =>
        IF input = '1' AND shift_right(counter_c, 10) < 18 THEN
          state <= s0;
        ELSIF input = '1' AND shift_right(counter_c, 10) >= 18 AND shift_right(counter_c, 10) <= 19 THEN
          counter_c <= (others => '0');
          state <= sL1;
        ELSIF shift_right(counter_c, 10) > 19 THEN
          state <= s0;
        END IF;
      WHEN sL1 =>
        IF input = '0' AND shift_right(counter_c, 10) < 8 THEN
          state <= s0;
        ELSIF input = '0' AND shift_right(counter_c, 10) >= 8 AND shift_right(counter_c, 10) <= 10 THEN
          counter_c <= (others => '0');
          y <= '1';
          state <= sY1;
        ELSIF shift_right(counter_c, 10) > 10 THEN
          state <= s0;
        END IF;
      WHEN sY1 =>
        IF shift_right(counter_c, 15) > 13 THEN
          counter_c <= (others => '0');
          y <= '0';
          state <= sY2;
        END IF;
      WHEN sY2 =>
        IF shift_right(counter_c, 15) > 127 THEN
          state <= s0;
        END IF;
      END CASE;
    END IF;
  END PROCESS;
END rtl;
