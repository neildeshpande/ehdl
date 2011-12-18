
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	n : in  std_logic_vector(7 downto 0);
	c : out std_logic_vector(7 downto 0));

end main;

architecture e_main of  main is 



signal fact_r0, fact_r2, fact_r1 : std_logic_vector(15 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(1,16);
signal c_r0, c_r2, c_r1 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);
signal n_r0, n_r2, n_r1 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);


begin
n_r0 <= n;
c <= c_r2;

--Pos--
process(clk,rst)
begin
if rst = '0' then
n_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,8);
fact_r1 <= ieee.std_logic_arith.conv_std_logic_vector(1,16);
elsif clk'event and clk = '1' then
if ((n_r1) > (1))then
if 1 /= 0  then
		fact_r1 <= ((fact_r1(7 downto 0)) * (n_r1));
		n_r1 <= ((n_r1) - (1));
end if;
else
if 1 /= 0  then
n_r1 <= n_r0;
fact_r1 <= fact_r0;
end if;
end if;
end if;
end process;

--Pos--
process(clk,rst)
begin
if rst = '0' then
n_r2 <= ieee.std_logic_arith.conv_std_logic_vector(0,8);
fact_r2 <= ieee.std_logic_arith.conv_std_logic_vector(1,16);
elsif clk'event and clk = '1' then
if ((n_r1) = (1)) then
n_r2 <= n_r1;
fact_r2 <= fact_r1;
end if;
end if;
end process;


	process (fact_r2)
	begin
		c_r2 <= fact_r2(7 downto 0);

	end process;



end e_main;

