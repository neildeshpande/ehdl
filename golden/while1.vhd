
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	a : in  std_logic_vector(31 downto 0);
	b : in  std_logic_vector(31 downto 0);
	c : out std_logic_vector(31 downto 0);
	d : out std_logic_vector(31 downto 0));

end main;

architecture e_main of  main is 



signal d_r0, d_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal c_r0, c_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal b_r0, b_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal a_r0, a_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);


begin
a_r0 <= a;
b_r0 <= b;
c <= c_r1;
d <= d_r1;

		c_r0 <= ieee.std_logic_arith.conv_std_logic_vector(0,32);
--Pos--
c_r1(31 downto 4) <= c_r0(31 downto 4);
c_r1(0 downto 0) <= c_r0(0 downto 0);
b_r1 <= b_r0;
a_r1 <= a_r0;
process(clk,rst)
begin
if rst = '0' then
c_r1(3 downto 1) <= ieee.std_logic_arith.conv_std_logic_vector(0,3);
elsif clk'event and clk = '1' then
if ((a_r1) = (1))then
if 1 /= 0  then
		c_r1(3 downto 1) <= ieee.std_logic_arith.conv_std_logic_vector(1,3);
end if;
else
if 1 /= 0  then
c_r1(3 downto 1) <= c_r0(3 downto 1);
end if;
end if;
end if;
end process;



end e_main;

