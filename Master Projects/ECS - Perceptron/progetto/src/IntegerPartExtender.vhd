library ieee;
use ieee.std_logic_1164.all;

entity IntegerPartExtender is
    generic (
        INPUT_WIDTH : integer := 8; -- Width of the input signal
        MSB_REPEATS : integer := 4 -- Number of times to repeat the MSB
    );
    port (
        inp : in std_logic_vector(INPUT_WIDTH - 1 downto 0);
        outp : out std_logic_vector(MSB_REPEATS + INPUT_WIDTH - 1 downto 0)
    );
end entity IntegerPartExtender;

architecture bhv of IntegerPartExtender is
    signal msb_repeated : std_logic_vector(MSB_REPEATS - 1 downto 0);
begin
    msb_repeated <= (others => inp(INPUT_WIDTH - 1));
    outp <= msb_repeated & inp;
end architecture bhv;