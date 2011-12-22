
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



type ar_type is array (0 to 7) of std_logic_vector(31 downto 0);
signal ar_r0, ar_r1 : ar_type := (others => ieee.std_logic_arith.conv_std_logic_vector(1,32));
signal c_r0, c_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal b_r0, b_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal a_r0, a_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);


begin
a_r0 <= a;
b_r0 <= b;
c <= c_r1;


	process (a_r0, b_r0)
	begin
		ar_r0(ieee.std_logic_unsigned.conv_integer(a_r0(2 downto 0)))  <= b_r0;

	end process;

--Pos--
ar_r1(7) <= ar_r0(7);
ar_r1(6) <= ar_r0(6);
ar_r1(1) <= ar_r0(1);
ar_r1(0) <= ar_r0(0);
ar_r1(3) <= ar_r0(3);
ar_r1(4) <= ar_r0(4);
b_r1 <= b_r0;
a_r1 <= a_r0;
process(clk,rst)
begin
if rst = '0' then
ar_r1(5) <= ieee.std_logic_arith.conv_std_logic_vector(1,32);
ar_r1(2) <= ieee.std_logic_arith.conv_std_logic_vector(1,32);
elsif clk'event and clk = '1' then
if ((ar_r1(ieee.std_logic_unsigned.conv_integer(b_r1(1 downto 0)))) = (a_r1))then
if 1 /= 0  then
		ar_r1(2)  <= ((a_r1) + (b_r1));
		ar_r1(5)  <= ((b_r1) - (((a_r1) and (a_r1))));
end if;
else
if 1 /= 0  then
ar_r1(5) <= ar_r0(5);
ar_r1(2) <= ar_r0(2);
end if;
end if;
end if;
end process;



end e_main;

