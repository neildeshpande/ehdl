
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	a : in  std_logic_vector(31 downto 0);
	b : in  std_logic_vector(31 downto 0);
	c : out std_logic_vector(31 downto 0));

end main;

architecture e_main of  main is 



signal c_r0 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal b_r0 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal a_r0 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);


begin
a_r0 <= a;
b_r0 <= b;
c <= c_r0;


	process (a_r0, b_r0)
	begin
		c_r0(7 downto 0) <= ((a_r0(15 downto 8)) + (b_r0(31 downto 24)));

	end process;

		c_r0(31 downto 8) <= ieee.std_logic_arith.conv_std_logic_vector(0,24);


end e_main;

