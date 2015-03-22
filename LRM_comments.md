Answers to SE comments on EHDL LRM


Your definition of a program is good, but it should appear later, not
in "lexical conventions."

Ok.

It's a little odd to have some lowercase, some mixed-case, and some
upper-case keywords.  Why not be consistent?

Shall we use only lower-case?


It's not clear from your definition of int and uint types whether
their sizes are simply constants, compile-time constants (e.g., 5 +
3), or dynamic.

Sizes must be compile-time constants. We can't change the width of a bus at run time.


Be careful about endianness: does m(0:4) denote the lowest five bits
or the lowest five bits in reverse order?

We need to choose... VHDL uses m(0 to 4) to reverse the order w.r.t. m(4 downto 0).

() and [.md](.md) don't have (or need) any associativity or precedence.

Ok. I think the reason is because they are matched.

Your syntax definition of const isn't right: isn't "const" a keyword?
It looks like you wrote it as a non-terminal.

const is listed among the keywords. Probably he doesn't like rule 6.1. We will refine it.


Your definition of function looks a little odd: to get a sequence of
statements, don't you have to write two braces, e.g., (x,y)foo(w,z) {{
a; b; }}

It looks clear to me and I don't understand why we should use two braces. However I realized that we forgot to add locals declarators to the function body.

Trying to impose the constraints on POS statements at the syntactic
level is a little awkward.  Instead, I'd have a separate section
explaining the rules for where POS must, may, and must not occur.

Ok, we can elaborate this section.


This is closely related to the overall semantics of the language,
which you don't really discuss.  In particular, how about parallel
functions and communication between them?  Multiple assignments to the
same variable/argument?  I'm left wondering exactly what a lot of
programs mean.

Was the LRM supposed to discuss semantics too? However:
1. as we said, all statements are translated into a process, that means all statements are implicitly parallel. Maybe we need to add this to the LRM. Communication between process is made by shared variables, with the assumption that each variable can be written by one process only before being sampled. After a POS, another process could eventually write the same variable, because it will represent the output of the register, which is a different object. ASYNC allows the user to declare a variable which is always bound to the same object, independently on the POS (i.e. async variables are not sampled: you always read the same wire!)

2. Due to 1, we can't allow the user to assign the same variable more than once, unless there is a POS between the two assignments. If we want we could add a functionality to let the user describe a sequential block of code. This would correspond to an explicit process, but we don't really need it.

Do you think that adding these rules to the LRM fixes the issue raised by the professor?