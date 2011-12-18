
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;


entity main  is 

port (
	clk : in std_logic;
	rst : in std_logic;
	car : in  std_logic_vector(0 downto 0);
	farmGreen : out std_logic_vector(0 downto 0);
	hwGreen : out std_logic_vector(0 downto 0);
	hwYellow : out std_logic_vector(0 downto 0);
	farmYellow : out std_logic_vector(0 downto 0));

end main;

architecture e_main of  main is 



constant FDuration_r0, FDuration_r1 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(3,8);
constant YDuration_r0, YDuration_r1 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(2,8);
constant FY_r0, FY_r1 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(3,2);
constant FG_r0, FG_r1 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(2,2);
constant HY_r0, HY_r1 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(1,2);
constant HG_r0, HG_r1 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,2);
signal state_r0, state_r1 : std_logic_vector(1 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,2);
signal yCntr_r0, yCntr_r1 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);
signal fCntr_r0, fCntr_r1 : std_logic_vector(7 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,8);
signal farmYellow_r0, farmYellow_r1 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,1);
signal hwYellow_r0, hwYellow_r1 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,1);
signal hwGreen_r0, hwGreen_r1 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,1);
signal farmGreen_r0, farmGreen_r1 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,1);
signal car_r0, car_r1 : std_logic_vector(0 downto 0) := ieee.std_logic_arith.conv_std_logic_vector(0,1);


begin
car_r0 <= car;
farmGreen <= farmGreen_r1;
hwGreen <= hwGreen_r1;
hwYellow <= hwYellow_r1;
farmYellow <= farmYellow_r1;

		state_r0 <= HG_r0;
--Pos--
car_r1 <= car_r0;
process(clk,rst)
begin
if rst = '0' then
yCntr_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,8);
state_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,2);
hwYellow_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
hwGreen_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
farmYellow_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
farmGreen_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
fCntr_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,8);
elsif clk'event and clk = '1' then
if 1 /= 0 then
if 1 /= 0  then
		if (state_r1 = HG_r1) then 
		hwGreen_r1 <= ieee.std_logic_arith.conv_std_logic_vector(1,1);
		hwYellow_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
		farmGreen_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
		farmYellow_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
		if (((car_r1) = (1))) then 
		yCntr_r1 <= ieee.std_logic_arith.conv_std_logic_vector(1,8);
		state_r1 <= HY_r1;

		else
		end if;
		elsif (state_r1 = HY_r1) then 
		hwGreen_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
		hwYellow_r1 <= ieee.std_logic_arith.conv_std_logic_vector(1,1);
		farmGreen_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
		farmYellow_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
		yCntr_r1 <= ((yCntr_r1) + (1));
		if (((yCntr_r1) = (YDuration_r1))) then 
		fCntr_r1 <= ieee.std_logic_arith.conv_std_logic_vector(1,8);
		state_r1 <= FG_r1;

		else
		end if;
		elsif (state_r1 = FG_r1) then 
		hwGreen_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
		hwYellow_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
		farmGreen_r1 <= ieee.std_logic_arith.conv_std_logic_vector(1,1);
		farmYellow_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
		fCntr_r1 <= ((fCntr_r1) + (1));
		if (((((car_r1) = (0))) or (((fCntr_r1) = (FDuration_r1))))) then 
		yCntr_r1 <= ieee.std_logic_arith.conv_std_logic_vector(1,8);
		state_r1 <= FY_r1;

		else
		end if;
		elsif (state_r1 = FY_r1) then 
		hwGreen_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
		hwYellow_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
		farmGreen_r1 <= ieee.std_logic_arith.conv_std_logic_vector(0,1);
		farmYellow_r1 <= ieee.std_logic_arith.conv_std_logic_vector(1,1);
		yCntr_r1 <= ((yCntr_r1) + (1));
		if (((yCntr_r1) = (YDuration_r1))) then 
		state_r1 <= HG_r1;

		else
		end if;
		end if;
end if;
else
if 1 /= 0  then
yCntr_r1 <= yCntr_r0;
state_r1 <= state_r0;
hwYellow_r1 <= hwYellow_r0;
hwGreen_r1 <= hwGreen_r0;
farmYellow_r1 <= farmYellow_r0;
farmGreen_r1 <= farmGreen_r0;
fCntr_r1 <= fCntr_r0;
end if;
end if;
end if;
end process;



end e_main;

