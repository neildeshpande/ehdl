int(8) main(int(8) a, int(8) b){

int(8) result;
result = gcd(a,b);

return result;

}

int(8) gcd(int(8) a, int(8) b){

int(1) en;

	while (a != b) {
		if (a > b) 
			a = a - b;
		else
			b = b - a;
	POS;
	/*if POS is not here, exit with error: "Non-synthesizable combinational loop"
	The compiler must be able to reveal the combinational loop
	the while condition is made on registered a and b. When it doesn't hold, the inputs are sampled.
	Be careful while defining the scope of variables. In this case the condition a != b refers to the registered values and not to the arguments!
	Also the binary operations and the assignments are made on the registered a and b.
	We may assume that a loop containing a POS always behaves in this way.

	Notice that POS without enable, means always enabled
	The POS statement is always outside if - else*/
	}
	
	if (a==b) 
	 en = 1;
        else 
	 en = 0;
	/*returns 1 or 0, not true or false. a and b refer to the ones inside the loop, already registered! Again be careful to the scope!*/
		
	POS(en);
	/* since it is not possible to initialize a and b, this register will be reset to 0 (default)
	   meaning that it's value is undefined until the en turn to 1. This is perfectly fine.
	   To have a reset, the user must declare a new variable, initialized to a constant value! */

	return b;
	/* this b is the b sampled by both POS(1) and POS(en)!!!!
}
