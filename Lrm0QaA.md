= Introduction = Possible answer to Mashooq questions about the LRM

Add your content here.


# Details #

Here are some other comments:

1) Paolo and I had a gmail chat that I posted on the wiki, please  take a look.

2) What happens when we type in "break" inside a while loop? Do we allow it?

-> I think we should not allow "break" inside loops. The synthesizer will   complain.
Again if n components are instantiated, they are there and you can't decide at run time
the value of n. Breaking a loop under a certain condition, which is not predictable statically is the same as changing the number of iterations dynamically. Instead, if the user wants to get the output only when the result is correct, he has to follow the gcd example, putting a POS(en) outside the loop that filters the temporary results.

3) Should we allow something like a = b(4:7)?  What about a(1:4) = b(0:3) ?

--> If you look at at the mips example (I still need to make some changes), sometimes it is a useful feature.
VHDL allows a(1:4) = b(0:3), but it raise a Warning, because you are reversing the bit order. If you know it is correct, you can do it.

4) For the scanner: do we need a "default" keyword for switch cases and
also "enum" ? Are we gonna have "typedef" in our language?

--> I think we said during our meetings, including Sunday:
YES "default", "enum"
NO "typedef"

We do not allow anonymous struct and we use the name of the struct as returning type for the functions or as a pseudo-type declaretor. When using a struct we know that our compiler will take each variable inside the struct (struct\_ID.variable\_ID) and put them in the place of the struct (mainly output of a component, but eventually, input too id
the struct is used to pass many arguments to a function).

5) If we are using "int" as a fundamental type, we probably  need
negative numbers so need an optional '-' in the front.

--> Neal pointed out the problem. We need to handle unary minus.

6) I noticed that there is a "bus" type in ast.mli. Do we need this if
we have arrays ? I think a bus can be easily expressed in terms of an
array.

--> The bus is our int(32). It is a collection of wires, interpreted as a single signal.
We chose the name "bus" on Sunday, because it suits the object we are representing.

7) We can have just one type of comment. ( /**...**/) . When possible,
let's try to get rid of duplicates.

--> If you prefer it's ok, we can remove the line comment. Anyway, if you look at the scanner.mll, both the types of comments are already handled.

8) Do we let the user define hex literals (0x1F)? I vote "no" for now.

--> I agree. Eventually we could include a way to assign a bus using hex literals in the "standard library" if we have time.