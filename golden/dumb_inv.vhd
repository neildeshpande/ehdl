
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	a : in  std_logic_vector(0 downto 0);
	b : out std_logic_vector(0 downto 0));

end main;

architecture e_main of  main is 



signal b_r0 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,1);
signal a_r0 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,1);


begin
a_r0 <= a;
b <= b_r0;


	process (a_r0)
	begin
		if (((a_r0) = (1))) then 
		b_r0 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);

		else
		b_r0 <= ieee.std_logic_arith.conv_std_logic_vector(1,1);
		end if;

	end process;



end e_main;

