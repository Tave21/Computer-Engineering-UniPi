library IEEE;
use IEEE.std_logic_1164.all;
use work.utility.all;

entity GenericRegisters is
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

end GenericRegisters;

architecture struct of GenericRegisters is
  signal di_signal : sum_arr;

  component DFF_N is
    generic (Nbit : positive := 18);
    port (
      reset : in std_logic;
      clk : in std_logic;
      en : in std_logic;
      di : in std_logic_vector(Nbit - 1 downto 0);
      do : out std_logic_vector(Nbit - 1 downto 0)
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

begin
  REG : for i in 0 to (N_IN - 1) generate

    Extend : IntegerPartExtender
    generic map(
      INPUT_WIDTH => 2 * BW - 1,  
      MSB_REPEATS =>  NIN_BITS - 2 * INT_PART_BITS
    )
    port map(
      inp => inp(i),
      outp => di_signal(i) 
    );

    DFF : DFF_N
    generic map(Nbit => N)
    port map(
      reset => reset,
      clk => clk,
      en => val(0),
      di => di_signal(i),
      do => outp(i)
    );
  end generate;

end architecture;