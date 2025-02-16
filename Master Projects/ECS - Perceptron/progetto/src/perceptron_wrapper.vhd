library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.utility.all;

entity perceptron_wrapper is 
	port (
	    clk     : in std_logic ;
    	reset   : in std_logic ;
		x		: in x_arr;
		-- no weights here
		-- no bias here 
        v       : in std_logic_vector(0 downto 0); 
		y		: out std_logic_vector(NOUT - 1 downto 0);
        v_out   : out std_logic_vector(0 downto 0) 
	);
end perceptron_wrapper;

architecture rtl of perceptron_wrapper is 

	component perceptron is
  		port (
	    	clk     : in std_logic ;
    		reset   : in std_logic ;
			x		: in x_arr;
			w 		: in inputs;
            v       : in std_logic_vector(0 downto 0); 
			b 	    : in std_logic_vector(BW - 1 downto 0) ;
			y		: out std_logic_vector(NOUT - 1 downto 0);   
            v_out   : out std_logic_vector(0 downto 0) 
  		);
	end component ;

	signal y_wrapper : std_logic_vector(NOUT - 1 downto 0);

	begin 

		myperceptron : Perceptron 
		port map (
			clk => clk,
			reset => reset, 
			x => x,
			w => CONSTANT_WEIGHTS, 
			b => CONSTANT_BIAS,
            v => v,
			y => y_wrapper,
            v_out => v_out
		);
		
		y <= y_wrapper;

end architecture; 