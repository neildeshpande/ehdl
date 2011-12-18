
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity adder  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	a : in  std_logic_vector(1 downto 0);
	b : in  std_logic_vector(1 downto 0);
	c : out std_logic_vector(1 downto 0));

end adder;

architecture e_adder of  adder is 



signal c_r0 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,2);
signal b_r0 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,2);
signal a_r0 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,2);


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
	a : in  std_logic_vector(1 downto 0);
	b : in  std_logic_vector(1 downto 0);
	d : out std_logic_vector(1 downto 0));

end main;

architecture e_main of  main is 

component adder
port (
	clk : in std_logic;
	rst : in std_logic;
	a : in  std_logic_vector(1 downto 0);
	b : in  std_logic_vector(1 downto 0);
	c : out std_logic_vector(1 downto 0));
end component;



signal d_r0 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,2);
signal b_r0 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,2);
signal a_r0 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,2);


begin
a_r0 <= a;
b_r0 <= b;
d <= d_r0;

adder_d : adder port map (
		clk => clk,
 		rst => rst,
		a => a_r0,
		b => d_r0,
		c => d_r0);



end e_main;

