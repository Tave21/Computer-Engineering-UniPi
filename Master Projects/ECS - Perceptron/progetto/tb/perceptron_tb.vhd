library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.utility.all;

entity perceptron_tb is
end perceptron_tb;

architecture bhv of perceptron_tb is
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
	signal x_tb : x_arr := (others => "00000000");
	signal w_tb : inputs := (others => "000000000");
	signal v_tb : std_logic_vector(0 downto 0) := "0";
	signal bias_tb : std_logic_vector(BW - 1 downto 0) := "000000000";
	signal y_tb : std_logic_vector(NOUT - 1 downto 0);
	signal v_out_tb : std_logic_vector(0 downto 0);

	-----------------------------------------------------------------------------------
	-- Component to test (DUT) declaration
	-----------------------------------------------------------------------------------
	component perceptron is
		port (
			clk : in std_logic;
			reset : in std_logic;
			x : in x_arr;
			w : in inputs;
			v : in std_logic_vector(0 downto 0);
			b : in std_logic_vector(BW - 1 downto 0);
			y : out std_logic_vector(NOUT - 1 downto 0);
			v_out : out std_logic_vector(0 downto 0)
		);
	end component;

begin

	clk_tb <= not(clk_tb) or end_sim after T_CLK/2;
	end_sim <= '1' after 100 * T_CLK;

	p : perceptron
	port map(
		clk => clk_tb,
		reset => reset_tb,
		x => x_tb,
		w => w_tb,
		v => v_tb,
		b => bias_tb,
		y => y_tb,
		v_out => v_out_tb
	);

	test_process : process (clk_tb)
		variable i : integer := 0;
	begin
		if (rising_edge(clk_tb)) then
			if (i > 80 ) then
				v_tb <= "0";
			elsif (i mod 10 >= 0) and (i mod 10 < 6) then
				v_tb <= "1";
			elsif (i mod 10 >= 6) and (i mod 10 < 10) then
				v_tb <= "0";
			end if;

			case(i) is
				-- perceptron test in three cases of the activation function
				when 0 => reset_tb <= '0';
				when 10 => x_tb <= (others => "00110000");
				w_tb <= (others => "001110000");
				bias_tb <= "000000000";
				-- xi = 0.75, wi = 0.875, b = 0, total sum + bias = 6.5625  > 2 -> correct output: 1 
				when 20 => x_tb <= (others => "00000000");
				w_tb <= (others => "000000000");
				bias_tb <= "000000000";
				-- all is zero, so correct output is 0.5 
				when 30 => x_tb <= (others => "01000000");
				w_tb <= (others => "000000001");
				bias_tb <= "001000000";
				-- xi = 1, wi = 0.0078125,b= 0.5, total sum + bias = 0.078125+0.5 = 0.578125,f(0.578125)=  0.64453125 
				when 40 => x_tb <= (others => "11100000");
				w_tb <= (others => "010000000");
				bias_tb <= "110000000";
				--xi = -0.5, wi = 1, b = -1, total sum + bias= -6  < -2 so correct output 0 
				when 50 => x_tb <= (0 => "01000000", others => "00000000");
				w_tb <= (0 => "010000000", others => "000000000");
				bias_tb <= "010000000";
				--x(0) = 1, x(i) = 0 i > 0, w(0) = 1, w(i) = 0 i > 0, bias = 1, total sum + bias = 2 --> correct output 1 
				when 60 => x_tb <= (2 downto 0 => "01000000", others => "00000000");
				w_tb <= (2 downto 0 => "010000000", others => "000000000");
				bias_tb <= "110000000";
				-- x(2) = x(1) = x(0) = 1, x(i) = 0 i > 2, w(2) = w(1) = w(0) = 1, w(i) = 0 i > 2, bias = -1, total sum + bias = 2 --> correct output 1 
				when 70 => x_tb <= (1 downto 0 => "01000000", others => "00000000");
				w_tb <= (1 downto 0 => "010000000", others => "000000000");
				bias_tb <= "110100000";
				-- x(1) = x(0) = 1, x(i) = 0 i > 1,  w(1) = w(0) = 1, w(i) = 0 i > 1, bias = -0.75, total sum + bias = 1.25 --> f(1.25) = 0.8125
				when others => null;
			end case;
			i := i + 1;
		end if;
	end process;

end architecture;