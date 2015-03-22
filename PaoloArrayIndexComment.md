# Paolo and Mashooq's discussion about array indices

## Paolo and Mashooq's discussion about variables and array indices ##

3:43 PM me: hey

 Paolo: hi!

 me: u got 5 mins ?

3:44 PM Paolo: yep

 me: i got your gcd email, i'll look at it in a minute

  but before that

  questions:

  the scanner does not have bitwise operators

  shoulw we include them

  they are pretty fundamental to hw design

3:45 PM Paolo: yes, but actually, the common &&, ||, ^, etc. are logical operators for the user

  while for hardware they are bitwise

  there's no difference

  except for conditions

3:46 PM me: ok so how are we gonna and together ifs ?

  cos && is usually used for that

  if conditions I mean

 Paolo: yes

  because in vhdl you have "and" "or" "xor"

3:47 PM they are bitwise operators, but they are the only ones available

  you may say " if a = '1' and b = '0' then"

3:48 PM me: yeah

 Paolo: the synth tool will translate it into (a and b')

3:49 PM and will use the signal to set a MUX

 me: so we probably need "And" and "or" as special keywords

 Paolo: no

  we can use && -> and

  || -> or

  ^ -> xor

  ! -> not

  it's a one to one relation

3:53 PM me: ok you are right

  second question

3:54 PM say we have an 32 bit input signal

3:55 PM ok nevermind

3:56 PM Paolo: ?

3:57 PM me: I was wondering if we should give the users a "var" keyword

  will it make our translation job easier

4:00 PM Paolo: I think it would make it more difficult

  then we have to understand if the user is declaring a variable erroneusly

4:01 PM sorry erroneously

 me: what if they want to do something like

  a = input(0:3)

4:02 PM and use a immendiately

  something you can do in VHDL i believe

  with var

4:03 PM Paolo: yes, but you do it also with signals, if they are inside different processes

  is you type in VHDL

a <= input(3 downto 0);

b <= a + conv\_std\_logic\_vector(1,4);

4:04 PM b takes always the value assigned to a in the first statement

  the problem is in the case of a process:

4:05 PM process (clk)

begin

if clk'event and clk = '1' then

a <= input(3 downto 0);

b <= a + conv\_std\_logic\_vector(1,4);

end if;

end process;

4:06 PM in this case b takes the value of a sampled at the clock raising edge

  and if you want b to use the value assigned to a, a must be a variable, which is declared inside the process and

> whose scope is limited to the process

4:07 PM but we are not leaving the user the possibility to write a process

4:08 PM so a variable is needed only when we have to use it in a loop

4:09 PM that mus be executed in sequence

  actually, we may need a keyword, such as index

  to distinguish between true signals and indexes

  or, even better

4:10 PM we may say that the index does not need to be declared before the loop

  VHDL, in fact, doesn't have a statement for index declaration

4:11 PM say for(i=0;i<N;i=i+1);

  and no other wires in the scope must have the same ID I

4:13 PM me: yeah

  I'll probably post your comments to google docs or forward it to the other guys, FYI

4:14 PM Paolo: post them to google code

  inside the wiki

 me: ok

 Paolo: thanks

4:15 PM I promise

  tomorrow night I'll post also an example of the other kind of loops

  unfortunately I have a pre-proposal due for tomorrow for formal verification

  and today I need to work on that

 me: ywah that's fine

4:16 PM you have already done a lot

 Paolo: I want also to go on with the parser and I'll proceed during the week

  I want ocamllex and ocamlyacc to compile with no complains by next monday!