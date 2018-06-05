dans typecheck
Premier passage trouver tous les calls de fonctions et resoudre les type parameters.
impl slice<T>{
    print<T>
}
fn ok<T>(T a){
    print (a);
}
hello(num b){
    ok(b);
}
fn main(){
    hello(2);
    ok('h');
}
en arrivant dans ok on dit que ok<T> implique de compiler print<T>
en arrivant dans hello on demande ok<num>
dans main on demande ok(char);
avoir une autre passe qui enleve tous les type arguments et crée des fonctions/résouds les appels.
on se retrouve avec les fonctions ok<num>, ok<char>, hello<num>, print<char>, print<num>

the typechecker indicates what needs to be compiled
It does so by generating requests.
There is a struct

une liste d'implications.
Une liste de requetes.

dans l'ordre :
refactor functions (un seul truc pour nativ