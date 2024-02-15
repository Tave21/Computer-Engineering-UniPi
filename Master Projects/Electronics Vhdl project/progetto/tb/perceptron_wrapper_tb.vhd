library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.utility.all;

entity perceptron_wrapper_tb is
end perceptron_wrapper_tb;

architecture bhv of perceptron_wrapper_tb is

	-----------------------------------------------------------------------------------
	-- Testbench constants
	-----------------------------------------------------------------------------------
	constant T_CLK : time := 20 ns;
	-----------------------------------------------------------------------------------
	-- Testbench signals
	-----------------------------------------------------------------------------------    
	signal clk_tb : std_logic := '0';
	signal reset_tb : std_logic := '1';
	signal end_sim : std_logic := '0';
	signal x_tb : x_arr:= (others => "00000000");
	signal v_tb : std_logic_vector(0 downto 0) := "0";
	signal y_tb : std_logic_vector(NOUT - 1 downto 0);
	signal v_out_tb : std_logic_vector(0 downto 0);

	-----------------------------------------------------------------------------------
	-- Component to test (DUT) declaration
	-----------------------------------------------------------------------------------
	component perceptron_wrapper is 
		port (
	        clk     : in std_logic ;
    	    reset   : in std_logic ;
		    x		: in x_arr;
            v       : in std_logic_vector(0 downto 0); 
		    y		: out std_logic_vector(NOUT - 1 downto 0);
            v_out   : out std_logic_vector(0 downto 0) 
		);
	end component;  

	begin 

		clk_tb <= not(clk_tb) or end_sim after T_CLK/2;
		end_sim <= '1' after 100 * T_CLK;

		pw: perceptron_wrapper 
		port map (
			clk => clk_tb,
			reset => reset_tb,
			x => x_tb,
            v => v_tb,
			y => y_tb,
            v_out => v_out_tb
		);

		test_process: process(clk_tb)
			variable i : integer := 0;
			begin 
            

				if (rising_edge(clk_tb)) then	
			        if (i > 39 ) then
				     v_tb <= "0";
			        elsif (i mod 10 >= 0) and (i mod 10 < 6) then
			        	v_tb <= "1";
			        elsif (i mod 10 >= 6) and (i mod 10 < 10) then
			        	v_tb <= "0";
			        end if;
					case(i) is
                        -- tests in three different area of activation function
						when 0 => reset_tb <= '0';
						when 10 => x_tb <= (others => "00001000");
                        -- x(i) = 0.125, w(i) = 0.5, b = 0.5 => totalsum + bias = 0.625 + 0.5 = 1.125,f(1.125) = 0.78125
						when 20 => x_tb <= (others => "01000000"); 
                        -- x(i) = 1, w(i) = 0.5, b = 0.5 => totalsum + bias = 5.5 > 2 -> correct output: 1
						when 30 => x_tb <= (others => "11000000"); 		
                        -- x(i) = -1, w(i) = 0.5, b = 0.5 => totalsum + bias = -5.5 > 2 -> correct output: 0
						when others => null;
					end case;
				i := i + 1; 
				end if;
			end process;			

end architecture;