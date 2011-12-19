
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	m : in  std_logic_vector(31 downto 0);
	primes : out std_logic_vector(31 downto 0));

end main;

architecture e_main of  main is 



type a_type is array (0 to 199) of std_logic_vector(0 downto 0);
signal a_r0, a_r2, a_r1 : a_type := (others => ieee.std_logic_arith.conv_std_logic_vector(0,1));
signal sig_r0, sig_r2, sig_r1 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,1);
signal n_r0, n_r2, n_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(2,32);
signal k_r0, k_r2, k_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(2,32);
signal primes_r0, primes_r2, primes_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal m_r0, m_r2, m_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);


begin
m_r0 <= m;
primes <= primes_r2;

--Pos--
m_r1 <= m_r0;
process(clk,rst)
begin
if rst = '0' then
a_r1 <= (others => ieee.std_logic_arith.conv_std_logic_vector(0,1));
sig_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
n_r1 <= ieee.std_logic_arith.conv_std_logic_vector(2,32);
k_r1 <= ieee.std_logic_arith.conv_std_logic_vector(2,32);
elsif clk'event and clk = '1' then
if ((n_r1) <= (m_r1))then
if 1 /= 0  then
		if (((((a_r1(ieee.std_logic_unsigned.conv_integer(n_r1))) = (0))) and (((k_r1) <= (m_r1))))) then 
		k_r1 <= ((k_r1) + (n_r1));
		if (((k_r1) = (n_r1))) then 
		sig_r1 <= ieee.std_logic_arith.conv_std_logic_vector(1,1);

		else
		a_r1(ieee.std_logic_unsigned.conv_integer(k_r1))  <= ieee.std_logic_arith.conv_std_logic_vector(1,1);
		end if;

		else
		k_r1 <= ((n_r1) + (1));
		n_r1 <= ((n_r1) + (1));
		sig_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
		end if;
end if;
else
if 1 /= 0  then
a_r1 <= a_r0;
sig_r1 <= sig_r0;
n_r1 <= n_r0;
k_r1 <= k_r0;
end if;
end if;
end if;
end process;

--Pos--
process(clk,rst)
begin
if rst = '0' then
a_r2 <= (others => ieee.std_logic_arith.conv_std_logic_vector(0,1));
sig_r2 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
n_r2 <= ieee.std_logic_arith.conv_std_logic_vector(2,32);
m_r2 <= ieee.std_logic_arith.conv_std_logic_vector(0,32);
k_r2 <= ieee.std_logic_arith.conv_std_logic_vector(2,32);
elsif clk'event and clk = '1' then
if sig_r1 /= 0  then
a_r2 <= a_r1;
sig_r2 <= sig_r1;
n_r2 <= n_r1;
m_r2 <= m_r1;
k_r2 <= k_r1;
end if;
end if;
end process;


	process (n_r2)
	begin
		primes_r2 <= n_r2;

	end process;



end e_main;

