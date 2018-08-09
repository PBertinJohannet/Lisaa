problem : 
	need information about the parameters to compile slice.
hmmm
how do we plan the compilation already ?
have a list of parameters needed.
have a list of associated functions to compile : 

eg params : U, T
func : method<U, T>, metho2<U, U> etc...


alright so now the generics on casses...
so we have a class with a generic
everytime it is instanciated we create a new class with these generics etc...
now the problem becomes... add monomorphised classes and monomorphised functions ? or just tell the typechecker to ?
run a second time on it maybe...


ok shit...
    so we have typechecked all functions.
    to do the static dispatch...