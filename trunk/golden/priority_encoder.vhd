
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	sel : in  std_logic_vector(7 downto 0);
	code : out std_logic_vector(2 downto 0));

end main;

architecture e_main of  main is 



signal code_r0 : std_logic_vector(2 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,3);
signal sel_r0 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);


begin
sel_r0 <= sel;
code <= code_r0;


	process (sel_r0)
	begin
		if (sel_r0(0 downto 0) /= 0 ) then 
		code_r0 <= ieee.std_logic_arith.conv_std_logic_vector(0,3);

		else
		if (sel_r0(1 downto 1) /= 0 ) then 
		code_r0 <= ieee.std_logic_arith.conv_std_logic_vector(1,3);

		else
		if (sel_r0(2 downto 2) /= 0 ) then 
		code_r0 <= ieee.std_logic_arith.conv_std_logic_vector(2,3);

		else
		if (sel_r0(3 downto 3) /= 0 ) then 
		code_r0 <= ieee.std_logic_arith.conv_std_logic_vector(3,3);

		else
		if (sel_r0(4 downto 4) /= 0 ) then 
		code_r0 <= ieee.std_logic_arith.conv_std_logic_vector(4,3);

		else
		if (sel_r0(5 downto 5) /= 0 ) then 
		code_r0 <= ieee.std_logic_arith.conv_std_logic_vector(5,3);

		else
		if (sel_r0(6 downto 6) /= 0 ) then 
		code_r0 <= ieee.std_logic_arith.conv_std_logic_vector(6,3);

		else
		code_r0 <= ieee.std_logic_arith.conv_std_logic_vector(7,3);
		end if;
		end if;
		end if;
		end if;
		end if;
		end if;
		end if;

	end process;



end e_main;

