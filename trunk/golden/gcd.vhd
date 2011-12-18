
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	a : in  std_logic_vector(7 downto 0);
	b : in  std_logic_vector(7 downto 0);
	c : out std_logic_vector(7 downto 0));

end main;

architecture e_main of  main is 



signal c_r0, c_r2, c_r1 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);
signal b_r0, b_r2, b_r1 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);
signal a_r0, a_r2, a_r1 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);


begin
a_r0 <= a;
b_r0 <= b;
c <= c_r2;

--Pos--
process(clk,rst)
begin
if rst = '0' then
b_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,8);
a_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,8);
elsif clk'event and clk = '1' then
if ((a_r1) /= (b_r1))then
if 1 /= 0  then
		if (((a_r1) > (b_r1))) then 
		a_r1 <= ((a_r1) - (b_r1));

		else
		b_r1 <= ((b_r1) - (a_r1));
		end if;
end if;
else
if 1 /= 0  then
b_r1 <= b_r0;
a_r1 <= a_r0;
end if;
end if;
end if;
end process;

--Pos--
process(clk,rst)
begin
if rst = '0' then
b_r2 <= ieee.std_logic_arith.conv_std_logic_vector(0,8);
a_r2 <= ieee.std_logic_arith.conv_std_logic_vector(0,8);
elsif clk'event and clk = '1' then
if ((a_r1) = (b_r1)) then
b_r2 <= b_r1;
a_r2 <= a_r1;
end if;
end if;
end process;


	process (a_r2)
	begin
		c_r2 <= a_r2;

	end process;



end e_main;

