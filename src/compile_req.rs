use statement::FunctionDecl;
use types::{LisaaType, TypedVar};

/// sinon
/// Parcourir depuis le main.
/// chercher les appels de fonctions.
/// les ajouter a la queue
/// Continuer en bfs.
/// Parfait !
/// Si on arrive dans ok<U>() qui appelle print<U>() il faut verifier qu'on ne l'a pas déjà compilé.
///
///
/// Donc dans l'ordre :
/// Dans typecheck on remplace FunctionCall pour inclure le type des <T, U> et on enregistre.
///
/// Ensuite on appelle une deuxieme fois pour créer toutes les requetes compilateur.
/// Ici on parcours l'arbre et on rajoute des copies des fonctions necessaires dans une hashmap empty
///
/// On a donc 2 hashmaps :
/// Une qui est entrain d'être parcourue.
/// Une qui est déjà parcourue.
///
/// Ensuite le compilateur s'en bat les steaks des types car il les connait déjà.
///
/// Il faudrait d'abord créer l'atribut inline et rechercher les Add/Print/etc...
///
///
/// Quand on aura des constraints genre ok<T : Add> on devra remplacer tous les calls de Add.
type a = usize;
