library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

package utility is

	--  definition of constants
	constant INT_PART_BITS : integer := 2; -- number of bits for the integer part, because the max value of integer part of inputs is 1 ad minimum -1
	constant N_IN : integer := 10; -- number of in inputs
	constant BW : integer := 9; -- number of bits of w
	constant NOUT : integer := 16; -- number of bits of the output perceptron 
	-- Number of bits for the maximum value of the integer part of activation function's input
	constant NIN_BITS : natural := natural(ceil(log2(real(N_in + 2)))) + 1; --   5 bits in this case (I have to represent: 11)
	--   +2 because we have the bias
	constant FR_BITS : integer := (BW - INT_PART_BITS) * 2 - 1; -- number of bits of the fractional part after the multiplication
	constant ZERO : std_logic_vector(NIN_BITS + FR_BITS - 1 downto 0) := "000000000000000000"; -- zero 
	constant ONE : std_logic_vector(NIN_BITS + FR_BITS - 1 downto 0) := "000010000000000000"; -- one 
	constant SFIXED_0_5 : std_logic_vector(NIN_BITS + FR_BITS - 1 downto 0) := "000001000000000000"; -- offset
	constant TWO_IN_FPA : std_logic_vector(NIN_BITS + FR_BITS - 1 downto 0) := "000100000000000000"; -- 2 in fixed point arithmetic
	constant MINUS_TWO_IN_FPA : std_logic_vector(NIN_BITS + FR_BITS - 1 downto 0) := "111100000000000000"; -- 2 in fixed point arithmetic
	-- definition of useful types, one for weights and one for inputs and one for products

	type inputs is array(N_IN - 1 downto 0) of std_logic_vector(BW - 1 downto 0); 
	type x_arr is array(N_IN - 1 downto 0) of std_logic_vector(BW - 2 downto 0); 
	
	type prod_arr is array (N_IN - 1 downto 0) of std_logic_vector(2 * BW - 2 downto 0); -- bits =  bits of product of x(i) and w(i)

	type sum_arr is array (N_IN - 1 downto 0) of std_logic_vector(NIN_BITS + FR_BITS - 1 downto 0); -- array of Nin numbers of 18 bits
	--  5 bits for integer part because max values are -10 and 10 that can be represented on 5 bits in 2's complement
	--  13 bits for fraction part because the multiplication x(i) * w(i) has fractional part with number
	--  of bits equal to the sum of the fractional parts of x(i) and w(i) that are 6 and 7 bits respectively



	-- constant weights to reduce number of inputs in the wrapper (otherwise 199 bits input
	-- are too much for the zybo board that has 100 pins available maximum)
	constant CONSTANT_WEIGHTS : inputs := (others => "001000000"); --fixed at 0.5
	constant CONSTANT_BIAS : std_logic_vector(BW - 1 downto 0) := "001000000"; --fixed at 0.5

	-- I chose to let the weights be constant because I think that when realizing a network in HW it is 
	-- first of all tested and trained in SW, so the weights are already adjusted when realizing it in HW.



end package;