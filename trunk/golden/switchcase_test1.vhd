
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	c : in  std_logic_vector(31 downto 0);
	a : in  std_logic_vector(31 downto 0);
	b : in  std_logic_vector(31 downto 0);
	sel : in  std_logic_vector(2 downto 0);
	out2 : out std_logic_vector(31 downto 0);
	out1 : out std_logic_vector(31 downto 0);
	out3 : out std_logic_vector(31 downto 0));

end main;

architecture e_main of  main is 



constant BLACK_r0 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(20,32);
constant RED_r0 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(10,32);
signal m1_r0 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(2,32);
signal m2_r0 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(4,32);
signal out3_r0 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal out1_r0 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal out2_r0 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal sel_r0 : std_logic_vector(2 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,3);
signal b_r0 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal a_r0 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal c_r0 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);


begin
c_r0 <= c;
a_r0 <= a;
b_r0 <= b;
sel_r0 <= sel;
out2 <= out2_r0;
out1 <= out1_r0;
out3 <= out3_r0;


	process (a_r0, b_r0, m2_r0, m1_r0)
	begin
		if (((a_r0) + (b_r0)) = 0) then 
		out1_r0 <= m1_r0;
		out2_r0 <= m2_r0;
		elsif (((a_r0) + (b_r0)) = 1) then 
		out1_r0 <= m2_r0;
		out2_r0 <= m1_r0;
		elsif (((a_r0) + (b_r0)) = 2) then 
		out1_r0 <= ((m1_r0) + (2));
		else 
		out2_r0 <= ((m1_r0) + (10));
		end if;

	end process;


	process (sel_r0)
	begin
		if (sel_r0 = 0) then 
		out3_r0 <= RED_r0;
		elsif (sel_r0 = 1) then 
		out3_r0 <= BLACK_r0;
		end if;

	end process;



end e_main;

