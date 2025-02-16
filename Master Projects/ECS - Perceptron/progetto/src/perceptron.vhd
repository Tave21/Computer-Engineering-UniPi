library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.utility.all;

entity perceptron is

  port (
    clk : in std_logic; -- clock of the system
    reset : in std_logic; -- Asynchronous reset - active high
    x : in x_arr;
    -- each element taken from x has :
    -- integer part: 2 bits --> because range is [-1,1], so in 2's complement I need 2 bits 
    -- fraction part : 6 bits --> 6 data bits (Nbit-integer part bits)
    w : in inputs;
    -- each element taken from w has :
    -- integer part: 2 bits --> because range is [-1,1], so in 2's complement I need 2 bits
    -- fraction part : 7 bits --> 7 data bits (Nbit-integer part bits)
    v : in std_logic_vector(0 downto 0); -- validity bit
    b : in std_logic_vector(BW - 1 downto 0); -- bias on BW bits
    y : out std_logic_vector(NOUT - 1 downto 0); -- output on NOUT bits 
    v_out : out std_logic_vector(0 downto 0) -- validity bit in output

  );

end entity;
  
architecture struct of perceptron is

  ----Internal signals declaration---------------------------------------------------------------------
  signal prod : prod_arr;
  signal prod_outreg : sum_arr;
  signal sum : sum_arr;
  signal totalsum : std_logic_vector(NIN_BITS + FR_BITS - 1 downto 0); -- 18 bits

  signal x_outreg : x_arr;
  signal b_extended : std_logic_vector(NIN_BITS + FR_BITS - 1 downto 0); -- 18 bits
  signal b_outreg : std_logic_vector(NIN_BITS + FR_BITS - 1 downto 0); -- 18 bits

  signal w_outreg : inputs;

  signal v_outreg : std_logic_vector(0 downto 0);
  signal v_outreg_two : std_logic_vector(0 downto 0);
  signal v_outreg_three : std_logic_vector(0 downto 0); -- 1 bit
  signal v_outreg_four : std_logic_vector(0 downto 0); -- 1 bit
  signal y_toRegister : std_logic_vector(NIN_BITS + FR_BITS - 1 downto 0); --18 bits
  signal y_out : std_logic_vector(NIN_BITS + FR_BITS - 1 downto 0); --18 bits

  --in the sum and product signal we have:
  --  5 bits for integer part because max values are -10 and 10 that can be represented on 5 bits in 2's complement
  --  13 bits for fraction part because the multiplication x(i) * w(i) has fractional part with number
  --    of bits equal to the sum of the fractional parts of x(i) and w(i) that are 6 and 7 bits respectively

  -----------------------------------------------------------------------------------------------------
  --Components declaration---------------------------------------------------------------------------
  component GenericRegisters is
    generic (
      N : positive := 18
    );
    port (
      clk : in std_logic;
      reset : in std_logic;
      inp : in prod_arr;
      val : in std_logic_vector(0 downto 0);
      outp : out sum_arr
    );
  end component;

  component InputRegisters is
    generic (
      N : positive := 18
    );

    port (
      clk : in std_logic;
      reset : in std_logic;
      inp : in inputs;
      outp : out inputs
    );

  end component;

  component InputxRegisters is
    generic (
      N : positive := 18
    );

    port (
      clk : in std_logic;
      reset : in std_logic;
      inp : in x_arr;
      outp : out x_arr
    );
  end component;

  component SpecialExtender is
    generic (
      INPUT_WIDTH : integer := 8; -- Width of the input signal
      MSB_REPEATS : integer := 4; -- Number of times to repeat the MSB
      ZERO_APPENDS : integer := 4 -- Number of zeros to append at the end
    );
    port (
      inp : in std_logic_vector(INPUT_WIDTH - 1 downto 0);
      outp : out std_logic_vector(MSB_REPEATS + INPUT_WIDTH + ZERO_APPENDS - 1 downto 0)
    );
  end component;

  component IntegerPartExtender is
    generic (
      INPUT_WIDTH : integer := 8; -- Width of the input signal
      MSB_REPEATS : integer := 4 -- Number of times to repeat the MSB
    );
    port (
      inp : in std_logic_vector(INPUT_WIDTH - 1 downto 0);
      outp : out std_logic_vector(MSB_REPEATS + INPUT_WIDTH - 1 downto 0)
    );
  end component;

  component DFF_N is
    generic (
      Nbit : positive := 9
    );
    port (
      reset : in std_logic;
      clk : in std_logic;
      en : in std_logic;
      di : in std_logic_vector(Nbit - 1 downto 0);
      do : out std_logic_vector(Nbit - 1 downto 0)
    );
  end component;
  ----------------------------------------------------------------------------------------------- 

begin

  ---EXTENDING BIAS-----------------------------------------------------------------------------------------

  Extend_bias : SpecialExtender
  generic map(
    INPUT_WIDTH => BW, --9 in our case  
    MSB_REPEATS => NIN_BITS - INT_PART_BITS, -- integer part: from 2 to 5 bits
    -- 5      -    2  = 3
    ZERO_APPENDS => FR_BITS - (BW - INT_PART_BITS) -- fractional part: from 7 to 13 bits
    --13      - (9    -    2 ) = 6
  )
  port map(
    inp => b,
    outp => b_extended
  );
  --- 2 7 --> 5 13 in our case
  -----------------------------------------------------------------------------------------------------

  ---REGISTER INSTANTIATION--------------------------------------------------------------------------------
  xRegisters : InputxRegisters
  generic map(N => BW - 1) --8 bits
  port map(
    clk => clk,
    reset => reset,
    inp => x,
    outp => x_outreg
  );

  wRegisters : InputRegisters
  generic map(N => BW) -- 9 bits
  port map(
    clk => clk,
    reset => reset,
    inp => w,
    outp => w_outreg
  );

  biasRegister : DFF_N
  generic map(Nbit => NIN_BITS + FR_BITS) --18 bits
  port map(
    reset => reset,
    clk => clk,
    en => '1',
    di => b_extended,
    do => b_outreg
  );

  vRegister : DFF_N
  generic map(Nbit => 1)
  port map(
    reset => reset,
    clk => clk,
    en => '1',
    di => v,
    do => v_outreg
  );
  -----------------------------------------------------------------------------------------------------
  --FOR GENERATE TO COMPUTE EACH PRODUCT
  PRODUCTION : for i in 0 to N_IN - 1 generate
    --17 bits                              8 bits            9 bits
    prod(i) <= std_logic_vector(signed(x_outreg(i)) * signed(w_outreg(i))); --directly on 17 bits
    -- 4 + 13 = 17 bits
  end generate;

  --SAVING THE PRODUCTS IN REGISTER

  prodRegisters : GenericRegisters
  generic map(N => NIN_BITS + FR_BITS) -- 18 bits
  port map(
    clk => clk,
    reset => reset,
    inp => prod,
    val => v_outreg,
    outp => prod_outreg
  );

  v_two : DFF_N
  generic map(Nbit => 1)
  port map(
    reset => reset,
    clk => clk,
    en => '1',
    di => v_outreg,
    do => v_outreg_two
  );

  --FOR GENERATE TO COMPUTE THE SUM
  SUMMATION : for i in 0 to N_IN - 1 generate

    SUM1 : if i = 0 generate
      sum(0) <= std_logic_vector(signed(prod_outreg(0)) + signed(b_extended)); --sum(0) will take the first value of the sum
      --18                                 ext 18(repeat MSB)              18
    end generate;

    SUMi : if i > 0 generate
      sum(i) <= std_logic_vector(signed(sum(i - 1)) + signed(prod_outreg(i))); --sum(Nin - 1) will take the final value of the sum
      -- 5 e 13
    end generate;

  end generate;
  ---SAVING OUTPUT IN REGISTER
  v_three : DFF_N
  generic map(Nbit => 1)
  port map(
    reset => reset,
    clk => clk,
    en => '1',
    di => v_outreg_two,
    do => v_outreg_three
  );
  outRegister : DFF_N
  generic map(Nbit => NIN_BITS + FR_BITS) --18 bits
  port map(
    reset => reset,
    clk => clk,
    en => v_outreg_two(0),
    di => sum(N_IN - 1),
    do => totalsum
  );

  -- COMPUTING ACTIVATION FUNCTION
  activation_function : process (totalsum)

  begin

    if to_integer(signed(totalsum)) < to_integer(signed(MINUS_TWO_IN_FPA)) then
      y_toRegister <= ZERO; -- 18 bits
    elsif to_integer(signed(totalsum)) > to_integer(signed(TWO_IN_FPA)) then
      y_toRegister <= ONE; -- 18 bits
    else
      y_toRegister <= std_logic_vector(to_signed(to_integer((signed(totalsum) / 4) + signed(SFIXED_0_5)), NIN_BITS + FR_BITS));
    end if;
  end process;

  ---SAVING OUTPUT IN REGISTER
  v_four : DFF_N
  generic map(Nbit => 1)
  port map(
    reset => reset,
    clk => clk,
    en => '1',
    di => v_outreg_three,
    do => v_outreg_four
  );
  outputRegister : DFF_N
  generic map(Nbit => NIN_BITS + FR_BITS) --18 bits
  port map(
    reset => reset,
    clk => clk,
    en => v_outreg_three(0),
    di => y_toRegister,
    do => y_out
  );

  --FINAL OUTPUT
  y <= y_out(NIN_BITS + FR_BITS - 1 downto NIN_BITS + FR_BITS - NOUT); --17 downto 2, thus 16 bits
  v_out <= v_outreg_four;

end architecture;