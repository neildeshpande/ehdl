
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	c : in  std_logic_vector(1 downto 0);
	a : in  std_logic_vector(1 downto 0);
	b : in  std_logic_vector(1 downto 0);
	sel : in  std_logic_vector(3 downto 0);
	res : out std_logic_vector(1 downto 0));

end main;

architecture e_main of  main is 



signal res_r0 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,2);
signal sel_r0 : std_logic_vector(3 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,4);
signal b_r0 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,2);
signal a_r0 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,2);
signal c_r0 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,2);


begin
c_r0 <= c;
a_r0 <= a;
b_r0 <= b;
sel_r0 <= sel;
res <= res_r0;


	process (sel_r0, c_r0, a_r0, b_r0)
	begin
		if (sel_r0 = 0) then 
		res_r0 <= ((((a_r0) + (b_r0))) + (c_r0));
		elsif (sel_r0 = 1) then 
		res_r0 <= ((((a_r0) - (b_r0))) + (c_r0));
		elsif (sel_r0 = 2) then 
		res_r0 <= ((c_r0) + (a_r0));
		elsif (sel_r0 = 3) then 
		res_r0 <= ((a_r0) and (b_r0));
		elsif (sel_r0 = 4) then 
		res_r0 <= ((a_r0) or (b_r0));
		elsif (sel_r0 = 5) then 
		res_r0 <= ((a_r0) xor  (b_r0));
		elsif (sel_r0 = 6) then 
		res_r0 <= (to_stdlogicvector( to_bitvector(a_r0) sll (conv_integer(b_r0)) ));
		elsif (sel_r0 = 7) then 
		res_r0 <= (to_stdlogicvector( to_bitvector(a_r0) srl (conv_integer(b_r0)) ));
		else 
		res_r0 <= ((((a_r0) + ((not b_r0)))) + (1));
		end if;

	end process;



end e_main;

