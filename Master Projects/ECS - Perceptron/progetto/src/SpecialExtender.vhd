library ieee;
use ieee.std_logic_1164.all;

entity SpecialExtender is
    generic (
        INPUT_WIDTH : integer := 8; -- Width of the input signal
        MSB_REPEATS : integer := 4; -- Number of times to repeat the MSB
        ZERO_APPENDS : integer := 4 -- Number of zeros to append at the end
    );
    port (
        inp : in std_logic_vector(INPUT_WIDTH - 1 downto 0);
        outp : out std_logic_vector(MSB_REPEATS + INPUT_WIDTH + ZERO_APPENDS - 1 downto 0)
    );
end entity SpecialExtender;

architecture Behavioral of SpecialExtender is
    signal msb_repeated : std_logic_vector(MSB_REPEATS - 1 downto 0);
    signal zeros_appended : std_logic_vector(ZERO_APPENDS - 1 downto 0);
begin
    msb_repeated <= (others => inp(INPUT_WIDTH - 1));
    zeros_appended <= (others => '0');
    outp <= msb_repeated & inp & zeros_appended;
end architecture Behavioral;