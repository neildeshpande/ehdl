
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



signal d_r0, d_r3, d_r2, d_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal c_r0, c_r3, c_r2, c_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal b_r0, b_r3, b_r2, b_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal a_r0, a_r3, a_r2, a_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);


begin
a_r0 <= a;
b_r0 <= b;
c <= c_r3;
d <= d_r3;

		c_r0(31 downto 30) <= ieee.std_logic_arith.conv_std_logic_vector(0,2);
		c_r0(19 downto 4) <= ieee.std_logic_arith.conv_std_logic_vector(0,16);
--Pos--
process(clk,rst)
begin
if rst = '0' then
c_r1(31 downto 30) <= ieee.std_logic_arith.conv_std_logic_vector(0,2);
c_r1(19 downto 4) <= ieee.std_logic_arith.conv_std_logic_vector(0,16);
b_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,32);
a_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,32);
elsif clk'event and clk = '1' then
if 1 /= 0  then
c_r1(31 downto 30) <= c_r0(31 downto 30);
c_r1(19 downto 4) <= c_r0(19 downto 4);
b_r1 <= b_r0;
a_r1 <= a_r0;
end if;
end if;
end process;

--Pos--
c_r2(31 downto 30) <= c_r1(31 downto 30);
c_r2(19 downto 4) <= c_r1(19 downto 4);
b_r2 <= b_r1;
a_r2 <= a_r1;
process(clk,rst)
begin
if rst = '0' then
c_r2(29 downto 20) <= ieee.std_logic_arith.conv_std_logic_vector(0,10);
c_r2(3 downto 1) <= ieee.std_logic_arith.conv_std_logic_vector(0,3);
c_r2(3 downto 1) <= ieee.std_logic_arith.conv_std_logic_vector(0,3);
d_r2 <= ieee.std_logic_arith.conv_std_logic_vector(0,32);
elsif clk'event and clk = '1' then
if ((a_r2) = (1))then
if 1 /= 0  then
		if (((b_r2(1 downto 0)) = (a_r2(1 downto 0)))) then 
		c_r2(3 downto 1) <= a_r2(3 downto 1);
		d_r2 <= ((a_r2) + (b_r2));

		else
		c_r2(29 downto 20) <= b_r2(29 downto 20);
		c_r2(3 downto 1) <= b_r2(3 downto 1);
		d_r2 <= ieee.std_logic_arith.conv_std_logic_vector(0,32);
		end if;
end if;
else
if 1 /= 0  then
c_r2(29 downto 20) <= c_r1(29 downto 20);
c_r2(3 downto 1) <= c_r1(3 downto 1);
c_r2(3 downto 1) <= c_r1(3 downto 1);
d_r2 <= d_r1;
end if;
end if;
end if;
end process;

--Pos--
c_r3(31 downto 30) <= c_r2(31 downto 30);
c_r3(19 downto 6) <= c_r2(19 downto 6);
b_r3 <= b_r2;
a_r3 <= a_r2;
process(clk,rst)
begin
if rst = '0' then
c_r3(5 downto 4) <= ieee.std_logic_arith.conv_std_logic_vector(0,2);
c_r3(29 downto 20) <= ieee.std_logic_arith.conv_std_logic_vector(0,10);
c_r3(3 downto 1) <= ieee.std_logic_arith.conv_std_logic_vector(0,3);
c_r3(3 downto 1) <= ieee.std_logic_arith.conv_std_logic_vector(0,3);
d_r3 <= ieee.std_logic_arith.conv_std_logic_vector(0,32);
elsif clk'event and clk = '1' then
if ((a_r3) = (1))then
if 1 /= 0  then
c_r3(29 downto 20) <= c_r2(29 downto 20);
c_r3(3 downto 1) <= c_r2(3 downto 1);
c_r3(3 downto 1) <= c_r2(3 downto 1);
d_r3 <= d_r2;
		c_r3(5 downto 4) <= a_r3(5 downto 4);
end if;
else
if 1 /= 0  then
c_r3(29 downto 20) <= c_r2(29 downto 20);
c_r3(3 downto 1) <= c_r2(3 downto 1);
c_r3(3 downto 1) <= c_r2(3 downto 1);
d_r3 <= d_r2;
c_r3(5 downto 4) <= c_r2(5 downto 4);
end if;
end if;
end if;
end process;



end e_main;

