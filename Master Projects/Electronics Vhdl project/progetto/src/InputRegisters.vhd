
library IEEE;
use IEEE.std_logic_1164.all;
use work.utility.all;

entity InputRegisters is
  generic (
    N : positive :=18
  );

  port (
    clk     : in std_logic ;
    reset   : in std_logic ;
    inp     : in inputs;   
    outp    : out inputs 
  ) ;

end InputRegisters;

architecture struct of InputRegisters is

    component DFF_N is
	    generic( Nbit : positive := 18); 
        port (
            reset	: in std_logic ;
            clk	    : in std_logic ;
            en	    : in std_logic ;
            di	    : in std_logic_vector(Nbit-1 downto 0);
            do	    : out std_logic_vector(Nbit-1 downto 0)
        );
    end component;

begin

    REG: for i in 0 to (N_in - 1) generate
        DFF : DFF_N 
        generic map (Nbit => N) 
        port map (
            reset   => reset, 
            clk     => clk, 
            en      => '1', 
            di      => inp(i), 
            do      => outp(i)
        );
    end generate ;

end architecture ;
