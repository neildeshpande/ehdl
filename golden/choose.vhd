
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	a : in  std_logic_vector(9 downto 0);
	choose : in  std_logic_vector(0 downto 0);
	b : in  std_logic_vector(9 downto 0);
	e : out std_logic_vector(9 downto 0);
	c : out std_logic_vector(9 downto 0);
	d : out std_logic_vector(9 downto 0);
	f : out std_logic_vector(9 downto 0));

end main;

architecture e_main of  main is 



signal f_r0 : std_logic_vector(9 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,10);
signal d_r0 : std_logic_vector(9 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,10);
signal c_r0 : std_logic_vector(9 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,10);
signal e_r0 : std_logic_vector(9 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,10);
signal b_r0 : std_logic_vector(9 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,10);
signal choose_r0 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,1);
signal a_r0 : std_logic_vector(9 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,10);


begin
a_r0 <= a;
choose_r0 <= choose;
b_r0 <= b;
e <= e_r0;
c <= c_r0;
d <= d_r0;
f <= f_r0;


	process (choose_r0, a_r0, b_r0)
	begin
		if (((choose_r0) = (1))) then 
		d_r0 <= b_r0;
		c_r0 <= a_r0;

		else
		d_r0 <= a_r0;
		c_r0 <= b_r0;
		end if;

	end process;


	process (a_r0, b_r0)
	begin
		e_r0 <= ((a_r0) + (b_r0));

	end process;


	process (a_r0, b_r0)
	begin
		f_r0 <= ((a_r0) - (b_r0));

	end process;



end e_main;

