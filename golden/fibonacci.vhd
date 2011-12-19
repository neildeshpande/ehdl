
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	n : in  std_logic_vector(7 downto 0);
	f : out std_logic_vector(7 downto 0));

end main;

architecture e_main of  main is 



signal a_r0, a_r2, a_r1 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);
signal b_r0, b_r2, b_r1 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(1,8);
signal fib_r0, fib_r2, fib_r1 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);
signal cntr_r0, cntr_r2, cntr_r1 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(1,8);
signal f_r0, f_r2, f_r1 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);
signal n_r0, n_r2, n_r1 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);


begin
n_r0 <= n;
f <= f_r2;

--Pos--
n_r1 <= n_r0;
process(clk,rst)
begin
if rst = '0' then
cntr_r1 <= ieee.std_logic_arith.conv_std_logic_vector(1,8);
b_r1 <= ieee.std_logic_arith.conv_std_logic_vector(1,8);
a_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,8);
elsif clk'event and clk = '1' then
if ((cntr_r1) < (n_r1))then
if 1 /= 0  then
		cntr_r1 <= ((cntr_r1) + (1));
		b_r1 <= ((a_r1) + (b_r1));
		a_r1 <= b_r1;
end if;
else
if 1 /= 0  then
cntr_r1 <= cntr_r0;
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
n_r2 <= ieee.std_logic_arith.conv_std_logic_vector(0,8);
cntr_r2 <= ieee.std_logic_arith.conv_std_logic_vector(1,8);
b_r2 <= ieee.std_logic_arith.conv_std_logic_vector(1,8);
a_r2 <= ieee.std_logic_arith.conv_std_logic_vector(0,8);
elsif clk'event and clk = '1' then
if ((cntr_r1) = (n_r1)) then
n_r2 <= n_r1;
cntr_r2 <= cntr_r1;
b_r2 <= b_r1;
a_r2 <= a_r1;
end if;
end if;
end process;


	process (a_r2)
	begin
		f_r2 <= a_r2;

	end process;



end e_main;

