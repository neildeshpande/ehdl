library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_signed.all;

entity main is
  
  port (
    a : in  std_logic_vector(31 downto 0);
    b : in  std_logic_vector(31 downto 0);
    c : out std_logic_vector(31 downto 0));

end main;

architecture ehdl of main is

begin  -- ehdl

  process (a,b)
  begin  -- process
    c <= a + b;
  end process;

end ehdl;
