
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity fulladder  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	b : in  std_logic_vector(0 downto 0);
	a : in  std_logic_vector(0 downto 0);
	carryin : in  std_logic_vector(0 downto 0);
	sum : out std_logic_vector(0 downto 0);
	carry : out std_logic_vector(0 downto 0));

end fulladder;

architecture e_fulladder of  fulladder is 



signal carry_r0 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,1);
signal sum_r0 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,1);
signal carryin_r0 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,1);
signal a_r0 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,1);
signal b_r0 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,1);


begin
b_r0 <= b;
a_r0 <= a;
carryin_r0 <= carryin;
sum <= sum_r0;
carry <= carry_r0;


	process (a_r0, b_r0, carryin_r0)
	begin
		sum_r0 <= ((((a_r0) xor  (b_r0))) xor  (carryin_r0));

	end process;


	process (carryin_r0, a_r0, b_r0)
	begin
		carry_r0 <= ((((a_r0) and (b_r0))) xor  (((carryin_r0) and (((a_r0) xor  (b_r0))))));

	end process;



end e_fulladder;


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	b : in  std_logic_vector(3 downto 0);
	a : in  std_logic_vector(3 downto 0);
	carryin : in  std_logic_vector(0 downto 0);
	s : out std_logic_vector(3 downto 0);
	overflow : out std_logic_vector(0 downto 0));

end main;

architecture e_main of  main is 

component fulladder
port (
	clk : in std_logic;
	rst : in std_logic;
	b : in  std_logic_vector(0 downto 0);
	a : in  std_logic_vector(0 downto 0);
	carryin : in  std_logic_vector(0 downto 0);
	sum : out std_logic_vector(0 downto 0);
	carry : out std_logic_vector(0 downto 0));
end component;



type sum_type is array (0 to 3) of std_logic_vector(0 downto 0);
signal sum_r0, sum_r2, sum_r1 : sum_type := (others => ieee.std_logic_arith.conv_std_logic_vector(0,1));
type carry_type is array (0 to 3) of std_logic_vector(0 downto 0);
signal carry_r0, carry_r2, carry_r1 : carry_type := (others => ieee.std_logic_arith.conv_std_logic_vector(0,1));
signal overflow_r0, overflow_r2, overflow_r1 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,1);
signal s_r0, s_r2, s_r1 : std_logic_vector(3 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,4);
signal carryin_r0, carryin_r2, carryin_r1 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,1);
signal a_r0, a_r2, a_r1 : std_logic_vector(3 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,4);
signal b_r0, b_r2, b_r1 : std_logic_vector(3 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,4);


begin
b_r0 <= b;
a_r0 <= a;
carryin_r0 <= carryin;
s <= s_r2;
overflow <= overflow_r2;

fulladder_carry0 : fulladder port map (
		clk => clk,
 		rst => rst,
		b => b_r0(0 downto 0),
		a => a_r0(0 downto 0),
		carryin => carryin_r0,
		sum => sum_r0(0),
		carry => carry_r0(0));

fulladder_carry1 : fulladder port map (
		clk => clk,
 		rst => rst,
		b => b_r0(1 downto 1),
		a => a_r0(1 downto 1),
		carryin => carry_r0(0),
		sum => sum_r0(1),
		carry => carry_r0(1));

--Pos--
process(clk,rst)
begin
if rst = '0' then
sum_r1(1) <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
sum_r1(0) <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
carry_r1(1) <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
carry_r1(0) <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
carryin_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
b_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,4);
a_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,4);
elsif clk'event and clk = '1' then
if 1 /= 0  then
sum_r1(1) <= sum_r0(1);
sum_r1(0) <= sum_r0(0);
carry_r1(1) <= carry_r0(1);
carry_r1(0) <= carry_r0(0);
carryin_r1 <= carryin_r0;
b_r1 <= b_r0;
a_r1 <= a_r0;
end if;
end if;
end process;

fulladder_carry2 : fulladder port map (
		clk => clk,
 		rst => rst,
		b => b_r1(2 downto 2),
		a => a_r1(2 downto 2),
		carryin => carry_r1(1),
		sum => sum_r1(2),
		carry => carry_r1(2));

fulladder_carry3 : fulladder port map (
		clk => clk,
 		rst => rst,
		b => b_r1(3 downto 3),
		a => a_r1(3 downto 3),
		carryin => carry_r1(2),
		sum => sum_r1(3),
		carry => carry_r1(3));

--Pos--
process(clk,rst)
begin
if rst = '0' then
sum_r2(3) <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
sum_r2(2) <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
sum_r2(1) <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
sum_r2(0) <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
carry_r2(3) <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
carry_r2(2) <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
carry_r2(1) <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
carry_r2(0) <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
carryin_r2 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
b_r2 <= ieee.std_logic_arith.conv_std_logic_vector(0,4);
a_r2 <= ieee.std_logic_arith.conv_std_logic_vector(0,4);
elsif clk'event and clk = '1' then
if 1 /= 0  then
sum_r2(3) <= sum_r1(3);
sum_r2(2) <= sum_r1(2);
sum_r2(1) <= sum_r1(1);
sum_r2(0) <= sum_r1(0);
carry_r2(3) <= carry_r1(3);
carry_r2(2) <= carry_r1(2);
carry_r2(1) <= carry_r1(1);
carry_r2(0) <= carry_r1(0);
carryin_r2 <= carryin_r1;
b_r2 <= b_r1;
a_r2 <= a_r1;
end if;
end if;
end process;


	process (sum_r2)
	begin
		s_r2(3 downto 3) <= sum_r2(3);

	end process;


	process (sum_r2)
	begin
		s_r2(2 downto 2) <= sum_r2(2);

	end process;


	process (sum_r2)
	begin
		s_r2(1 downto 1) <= sum_r2(1);

	end process;


	process (sum_r2)
	begin
		s_r2(0 downto 0) <= sum_r2(0);

	end process;


	process (a_r2, b_r2, sum_r2)
	begin
		if (((((((a_r2) > (0))) and (((b_r2) > (0))))) and (((sum_r2(3)) < (0))))) then 
		overflow_r2 <= ieee.std_logic_arith.conv_std_logic_vector(1,1);

		else
		if (((((((a_r2) < (0))) and (((b_r2) < (0))))) and (((sum_r2(3)) > (0))))) then 
		overflow_r2 <= ieee.std_logic_arith.conv_std_logic_vector(1,1);

		else
		overflow_r2 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
		end if;
		end if;

	end process;



end e_main;

