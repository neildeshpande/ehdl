1) List functions -> 	create entity interface
> create component instance interface
> create instance template.

A) 	Function map: key -> function name
> -> value function\_obj

> function\_obj: type Ast.func\_decl

B)	Globals map: key  -> name
> -> bus\_obj

> bus\_obj: type Ast.bus


2) list of local variables  and global constants per each function ->		signals list.
> symbol table


3) body eval ->		translate each expression or statement into a process

4) Types check ->	check assignments





int(32) c adder(int(32) a, int(32) b){

> c = a + b;

}


entity adder is
> port (
> > a : in  std\_logic\_vector(31 downto 0);
> > b : in  std\_logic\_vector(31 downto 0);
> > > c : out std\_logic\_vector(31 downto 0));
end adder;


component adder is

> port (
> > a : in  std\_logic\_vector(31 downto 0);
> > b : in  std\_logic\_vector(31 downto 0);
> > > c : out std\_logic\_vector(31 downto 0));
end component;