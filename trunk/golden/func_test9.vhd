
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity func1  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	a : in  std_logic_vector(1 downto 0);
	b : in  std_logic_vector(3 downto 0);
	c : out std_logic_vector(3 downto 0));

end func1;

architecture e_func1 of  func1 is 

component func2
port (
	clk : in std_logic;
	rst : in std_logic;
	a : in  std_logic_vector(1 downto 0);
	b : out std_logic_vector(3 downto 0));
end component;



signal m_r0 : std_logic_vector(3 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,4);
signal c_r0 : std_logic_vector(3 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,4);
signal b_r0 : std_logic_vector(3 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,4);
signal a_r0 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,2);


begin
a_r0 <= a;
b_r0 <= b;
c <= c_r0;

func2_m : func2 port map (
		clk => clk,
 		rst => rst,
		a => a_r0,
		b => m_r0);


	process (m_r0, b_r0)
	begin
		c_r0 <= ((m_r0) + (b_r0));

	end process;



end e_func1;


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity func2  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	a : in  std_logic_vector(1 downto 0);
	b : out std_logic_vector(3 downto 0));

end func2;

architecture e_func2 of  func2 is 



signal b_r0 : std_logic_vector(3 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,4);
signal a_r0 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,2);


begin
a_r0 <= a;
b <= b_r0;


	process (a_r0)
	begin
		b_r0(1 downto 0) <= a_r0;

	end process;



end e_func2;


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	b : in  std_logic_vector(3 downto 0);
	a : in  std_logic_vector(1 downto 0);
	c : in  std_logic_vector(1 downto 0);
	d : out std_logic_vector(3 downto 0));

end main;

architecture e_main of  main is 

component func1
port (
	clk : in std_logic;
	rst : in std_logic;
	a : in  std_logic_vector(1 downto 0);
	b : in  std_logic_vector(3 downto 0);
	c : out std_logic_vector(3 downto 0));
end component;



signal d_r0 : std_logic_vector(3 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,4);
signal c_r0 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,2);
signal a_r0 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,2);
signal b_r0 : std_logic_vector(3 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,4);


begin
b_r0 <= b;
a_r0 <= a;
c_r0 <= c;
d <= d_r0;

func1_d : func1 port map (
		clk => clk,
 		rst => rst,
		a => a_r0,
		b => b_r0,
		c => d_r0);



end e_main;

