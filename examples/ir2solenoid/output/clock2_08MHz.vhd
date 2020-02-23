ENTITY frame_b_left IS
PORT(clock: IN STD_LOGIC;
  reset: IN STD_LOGIC;
  input: IN STD_LOGIC;
  out: OUT_STD_LOGIC);
END frame_b_left;

ARCHITECTURE rtl OF frame_b_left IS
  TYPE StateType IS (s0,s1,s2,s3,s4);
  SIGNAL state: StateType;
  CONSTANT counter_c_max : INTEGER := 2 ** 22;
  SIGNAL counter_c : INTEGER RANGE 0 TO counter_c_max - 1 := 0;
BEGIN
  PROCESS (clock,reset)
  BEGIN
    IF (reset = '1') THEN
      state <= s0;
    ELSIF rising_edge(clock) THEN
      counter_c <= (counter_c + 1) MOD counter_c_max;
      CASE state IS
      WHEN s0 =>
        IF input = '0' THEN
          counter_c := 0;
          state <= s1;
        END IF
      WHEN s1 =>
        IF input = '0' AND shift_right(counter_c, 10) < 18 THEN
          state <= s1;
        ELSIF input = '1' AND shift_right(counter_c, 10) >= 18 AND shift_right(counter_c, 10) <= 18 THEN
          counter_c := 0;
          state <= s2;
        ELSE
          state <= s0;
        END IF
      WHEN s2 =>
        IF input = '1' AND shift_right(counter_c, 10) < 8 THEN
          state <= s2;
        ELSIF input = '0' AND shift_right(counter_c, 10) >= 8 AND shift_right(counter_c, 10) <= 9 THEN
          counter_c := 0;
          out <= '1';
          state <= s3;
        ELSE
          state <= s0;
        END IF
      WHEN s3 =>
        IF shift_right(counter_c, 15) < 12 THEN
          state <= s3;
        ELSE
          counter_c := 0;
          out <= '0';
          state <= s4;
        END IF
      WHEN s4 =>
        IF shift_right(counter_c, 15) < 126 THEN
          state <= s4;
        ELSE
          state <= s0;
        END IF
      END CASE;
    END IF;
  END PROCESS;
END rtl;
