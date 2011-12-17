
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	a : in  std_logic_vector(3 downto 0);
	b : in  std_logic_vector(3 downto 0);
	res : out std_logic_vector(3 downto 0));

end main;

architecture e_main of  main is 



signal res_r0 : std_logic_vector(3 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,4);
signal b_r0 : std_logic_vector(3 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,4);
signal a_r0 : std_logic_vector(3 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,4);


begin
a_r0 <= a;
b_r0 <= b;
res <= res_r0;


	process (a_r0)
	begin
		if (a_r0 /= 0 ) then 
		res_r0 <= ieee.std_logic_arith.conv_std_logic_vector(3,4);

		else
		res_r0 <= ieee.std_logic_arith.conv_std_logic_vector(4,4);
		end if;

	end process;



end e_main;

