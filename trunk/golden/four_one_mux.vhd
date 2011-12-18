
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	d : in  std_logic_vector(7 downto 0);
	b : in  std_logic_vector(7 downto 0);
	a : in  std_logic_vector(7 downto 0);
	c : in  std_logic_vector(7 downto 0);
	sel : in  std_logic_vector(1 downto 0);
	z : out std_logic_vector(7 downto 0));

end main;

architecture e_main of  main is 



signal z_r0 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);
signal sel_r0 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,2);
signal c_r0 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);
signal a_r0 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);
signal b_r0 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);
signal d_r0 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);


begin
d_r0 <= d;
b_r0 <= b;
a_r0 <= a;
c_r0 <= c;
sel_r0 <= sel;
z <= z_r0;


	process (sel_r0, a_r0, b_r0, c_r0, d_r0)
	begin
		if (sel_r0 = 0) then 
		z_r0 <= a_r0;
		elsif (sel_r0 = 1) then 
		z_r0 <= b_r0;
		elsif (sel_r0 = 2) then 
		z_r0 <= c_r0;
		else 
		z_r0 <= d_r0;
		end if;

	end process;



end e_main;

