
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	d : in  std_logic_vector(9 downto 0);
	b : in  std_logic_vector(9 downto 0);
	a : in  std_logic_vector(9 downto 0);
	c : in  std_logic_vector(9 downto 0);
	sel : in  std_logic_vector(2 downto 0);
	p : out std_logic_vector(9 downto 0);
	m : out std_logic_vector(9 downto 0);
	n : out std_logic_vector(9 downto 0);
	x : out std_logic_vector(9 downto 0));

end main;

architecture e_main of  main is 



signal x_r0 : std_logic_vector(9 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,10);
signal n_r0 : std_logic_vector(9 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,10);
signal m_r0 : std_logic_vector(9 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,10);
signal p_r0 : std_logic_vector(9 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,10);
signal sel_r0 : std_logic_vector(2 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,3);
signal c_r0 : std_logic_vector(9 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,10);
signal a_r0 : std_logic_vector(9 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,10);
signal b_r0 : std_logic_vector(9 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,10);
signal d_r0 : std_logic_vector(9 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,10);


begin
d_r0 <= d;
b_r0 <= b;
a_r0 <= a;
c_r0 <= c;
sel_r0 <= sel;
p <= p_r0;
m <= m_r0;
n <= n_r0;
x <= x_r0;


	process (sel_r0, a_r0, b_r0)
	begin
		if (((sel_r0) = (1))) then 
		n_r0 <= b_r0;
		m_r0 <= a_r0;

		else
		n_r0 <= a_r0;
		m_r0 <= b_r0;
		end if;

	end process;


	process (b_r0, a_r0)
	begin
		if (((a_r0) < (b_r0))) then 
		p_r0 <= a_r0;

		else
		end if;

	end process;


	process (a_r0, c_r0, sel_r0, b_r0)
	begin
		if (((a_r0) > (b_r0))) then 
		if (((((sel_r0) >= (3))) or (((a_r0) > (c_r0))))) then 
		x_r0 <= ieee.std_logic_arith.conv_std_logic_vector(1,10);

		else
		end if;

		else
		if (((((((b_r0) > (c_r0))) and (((sel_r0) >= (5))))) or (((sel_r0) <= (2))))) then 
		if (((b_r0) /= (6))) then 
		x_r0 <= ieee.std_logic_arith.conv_std_logic_vector(2,10);

		else
		x_r0 <= ieee.std_logic_arith.conv_std_logic_vector(3,10);
		end if;

		else
		x_r0 <= ieee.std_logic_arith.conv_std_logic_vector(4,10);
		end if;
		end if;

	end process;



end e_main;

