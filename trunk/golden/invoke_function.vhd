
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity adder  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	a : in  std_logic_vector(31 downto 0);
	b : in  std_logic_vector(31 downto 0);
	c : out std_logic_vector(31 downto 0));

end adder;

architecture e_adder of  adder is 



constant en_r0 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(1,1);
signal c_r0 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal b_r0 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal a_r0 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);


begin
a_r0 <= a;
b_r0 <= b;
c <= c_r0;


	process (a_r0, b_r0)
	begin
		c_r0 <= ((a_r0) + (b_r0));

	end process;



end e_adder;


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	e : in  std_logic_vector(31 downto 0);
	f : in  std_logic_vector(31 downto 0);
	d : out std_logic_vector(31 downto 0);
	c : out std_logic_vector(31 downto 0);
	g : out std_logic_vector(7 downto 0));

end main;

architecture e_main of  main is 

component adder
port (
	clk : in std_logic;
	rst : in std_logic;
	a : in  std_logic_vector(31 downto 0);
	b : in  std_logic_vector(31 downto 0);
	c : out std_logic_vector(31 downto 0));
end component;



constant en_r0, en_r2, en_r1 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(1,1);
type ar_type is array (0 to 2) of std_logic_vector(31 downto 0);
signal ar_r0, ar_r2, ar_r1 : ar_type := (others => ieee.std_logic_arith.conv_std_logic_vector(1,32));
signal dummy_r0, dummy_r2, dummy_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal g_r0, g_r2, g_r1 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);
signal c_r0, c_r2, c_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal d_r0, d_r2, d_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal f_r0, f_r2, f_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);
signal e_r0, e_r2, e_r1 : std_logic_vector(31 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,32);


begin
e_r0 <= e;
f_r0 <= f;
d <= d_r2;
c <= c_r2;
g <= g_r2;

adder_ar0 : adder port map (
		clk => clk,
 		rst => rst,
		a => e_r0,
		b => f_r0,
		c => ar_r0(0));

adder_aren : adder port map (
		clk => clk,
 		rst => rst,
		a => ar_r0(0),
		b => f_r0,
		c => ar_r0(ieee.std_logic_unsigned.conv_integer(en_r0)));

		ar_r0(2)  <= ieee.std_logic_arith.conv_std_logic_vector(2,32);
		dummy_r0 <= ieee.std_logic_arith.conv_std_logic_vector(3,32);
adder_c : adder port map (
		clk => clk,
 		rst => rst,
		a => ieee.std_logic_arith.conv_std_logic_vector(1,32),
		b => f_r0,
		c => c_r0);

--Pos--
e_r1 <= e_r0;
process(clk,rst)
begin
if rst = '0' then
ar_r1(2) <= ieee.std_logic_arith.conv_std_logic_vector(1,32);
ar_r1(1) <= ieee.std_logic_arith.conv_std_logic_vector(1,32);
ar_r1(0) <= ieee.std_logic_arith.conv_std_logic_vector(1,32);
f_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,32);
dummy_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,32);
c_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,32);
elsif clk'event and clk = '1' then
if ((1) = (1)) then
ar_r1(2) <= ar_r0(2);
ar_r1(1) <= ar_r0(1);
ar_r1(0) <= ar_r0(0);
f_r1 <= f_r0;
dummy_r1 <= dummy_r0;
c_r1 <= c_r0;
end if;
end if;
end process;

adder_d : adder port map (
		clk => clk,
 		rst => rst,
		a => f_r1,
		b => e_r1(31 downto 0),
		c => d_r1);


	process (e_r1)
	begin
		g_r1 <= e_r1(7 downto 0);

	end process;

--Pos--
e_r2 <= e_r1;
d_r2 <= d_r1;
process(clk,rst)
begin
if rst = '0' then
ar_r2(2) <= ieee.std_logic_arith.conv_std_logic_vector(1,32);
ar_r2(1) <= ieee.std_logic_arith.conv_std_logic_vector(1,32);
ar_r2(0) <= ieee.std_logic_arith.conv_std_logic_vector(1,32);
g_r2 <= ieee.std_logic_arith.conv_std_logic_vector(0,8);
f_r2 <= ieee.std_logic_arith.conv_std_logic_vector(0,32);
dummy_r2 <= ieee.std_logic_arith.conv_std_logic_vector(0,32);
c_r2 <= ieee.std_logic_arith.conv_std_logic_vector(0,32);
elsif clk'event and clk = '1' then
if ((en_r1) = (1)) then
ar_r2(2) <= ar_r1(2);
ar_r2(1) <= ar_r1(1);
ar_r2(0) <= ar_r1(0);
g_r2 <= g_r1;
f_r2 <= f_r1;
dummy_r2 <= dummy_r1;
c_r2 <= c_r1;
end if;
end if;
end process;



end e_main;

