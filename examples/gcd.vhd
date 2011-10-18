--gcd function--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;

entity gcd is
  
  port (
    -- standard clock and reset inputs
    clk  : in  std_logic;
    rst  : in  std_logic;
    -- function arguments (inputs)
    a          : in  std_logic_vector(7 downto 0);
    b    : in  std_logic_vector(7 downto 0);
    -- function returning variable (outputs)
    ret  : out std_logic_vector(7 downto 0));

end gcd;

architecture ehdl_gcd of gcd is

  --local variables declaration (plus eventually initialization)
  signal en : std_logic;

  --registers
  signal a_r : std_logic_vector(7 downto 0);
  signal b_r : std_logic_vector(7 downto 0);
  signal a_rr : std_logic_vector(7 downto 0);
  signal b_rr : std_logic_vector(7 downto 0);
  --must count how many times a variable (same identifier in EHDL) is sampled
  --that means how many POS are present inside the scope of that variable.

  --components list
  
begin  -- ehdl

  --While process. It is sequential, because of the POS.
 process (clk, rst)
  begin  -- process
    if rst = '0' then
        a_r <= (others => '0');
	b_r <= (others => '0');
    elsif clk'event and clk = '1' then  -- rising clock edge
    --While loop with register becomes an if!
    --While condition doesn't hold -> sample the input
      if (not (a_r /= b_r)) then
        a_r <= a;
        b_r <= b;
    --While condition holds -> sample the result of the body of the loop
      else
       if a_r > b_r then
         a_r <= a_r - b_r;
       else
        b_r <= b_r - a_r;
       end if;
      end if;
    end if;
  end process;

  --en assignment
  process (a_r, b_r) --list of all variables evaluated!!!
   begin
    if ( a_r = b_r ) then
	en <= '1';
    else
	en <= '0';
	  end if;
  end process;

  --POS(en)
  process(clk, rst)
   begin
     if rst = '0' then
	b_rr <= (others => '0');
     elsif clk'event and clk = '0' then
      if en = '1' then
	b_rr <= b_r;
      end if;
     end if;
  end process;

  --Output assignment
  ret <= b_rr;

end ehdl_gcd;



--Top Entity--
library ieee;
use ieee.std_logic_1164.all;

entity main is
  
 port (
    -- standard clock and reset inputs
    clk     : in  std_logic;
    rst     : in  std_logic;
    -- function arguments (inputs)
    a       : in  std_logic_vector(7 downto 0);
    b       : in  std_logic_vector(7 downto 0);
    -- function returning variable (outputs)
    ret     : out std_logic_vector(7 downto 0));

end main;


architecture ehdl_main of main is
  
  --local variables declaration
  signal result : std_logic_vector(7 downto 0);

  --registers


  --conponents list
  --gcd--
  component gcd
    port (
      clk       : in  std_logic;
      rst       : in  std_logic;
      a         : in  std_logic_vector(7 downto 0);
      b         : in  std_logic_vector(7 downto 0);
      ret       : out std_logic_vector(7 downto 0));
   end component;

  --gcd signal list; one per each instance!!!
  --need to count how many times the same function is called!
  signal gcd_0_a : std_logic_vector(7 downto 0);
  signal gcd_0_b : std_logic_vector(7 downto 0);
  signal gcd_0_ret : std_logic_vector(7 downto 0);
  
begin  -- beh

  result <= gcd_0_ret;
  --Component intance
	--Components inputs assignment
  	gcd_0_a <= a;
  	gcd_0_b <= b;

  gcd_0 : gcd
    port map (
      clk   => clk,
      rst   => rst,
      a     => gcd_0_a,
      b     => gcd_0_b,
      ret   => gcd_0_ret);

  --outputs assignment
  ret <= result;
  
end ehdl_main;



--PRAGMA translate_off

--Testbench - not automatically generated!!!!!

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;
use ieee.std_logic_arith.all;

entity gcd_wrap_tb is
  
end gcd_wrap_tb;

architecture tb of gcd_wrap_tb is


  signal a,b,ret : std_logic_vector(7 downto 0);
  signal clk, rst : std_logic := '0';
  constant h_period : time := 50 ns;

  component main
    port (
      clk   : in  std_logic;
      rst   : in  std_logic;
      a     : in  std_logic_vector(7 downto 0);
      b     : in  std_logic_vector(7 downto 0);
      ret   : out std_logic_vector(7 downto 0));
  end component;
  
begin  -- tb

  --CLOCK and RESET
  clk <= not clk after h_period;
  rst <= '1' after (4*h_period);
  
  --INPUTS
  a <= conv_std_logic_vector(37,8);
  b <= conv_std_logic_vector(55,8);

  main_0 : main
    port map (
      clk   => clk,
      rst   => rst,
      a     => a,
      b     => b,
      ret   => ret);


end tb;
