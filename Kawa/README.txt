Javier Peña Castaño
LDD3 MAG
javier.pena-castano@etu-upsaclay.fr


Exécution du Projet:
    Pour compiler on doit utiliser la commande dune build depuis le dossier "Kawa". 
    Vous pouvez compiler directement sans modifier le code.

    Au niveau des tests, pour son exécution, il faut utiliser la commande suivante: ./kawai.exe tests/nom_du_fichier.kwa 
    utiliser cette commande depuis le dossier "Kawa". Voici un exemple: ./kawai.exe tests/arith.kwa.
    
Utilisation des Fichiers de Test

    Le dossier "tests" contient au moin un fichier de test pour chaque partie du langage. (ce sont le fichiers de base données dans le squelette du code, avec quelques modfications).
    Ajout d'un test extension.kwa pour tester l'extension traitée.
    Dans les fichiers, on trouve des instructions commentées pour vérifier le comportement du typechecker.
    
Fonctionnement du Langage
    On peut déclarer de variables globales, locales, méthodes classes et attributs propres à chaque classe et des variables locales propres à chaque méthode.
    
    Toutes les variables globales déclarées sont initialisées à Null lors de l'exécution du programme.

    Lors de la création d'un nouvel objet ses attributs sont initialisés à Null et les constructeurs définis dans les classes peuvent modifier ces valeurs initiales en fonction des paramètres fournis.
    
    Au niveau de l'arithmétique, on utlise les opérateurs suivants: <, <=, >, >=, !, -, +, *, /, %, ==, !=, ===, =/=, &&, ||.
    On utlise == et != pour l'égalité physique et === et =/= pour l'égalité structurelle. Il y a deux types d'opérateurs -, un binaire (moins), et un unaire (oppposé).

    Les instructions sont return, if (...) else  et print. 
    Print peut afficher que des Int et des Bool, pour un Void il affiche un erreur avec le message "cannot print non-integer or non-boolean value".
    Les conditions des if et while  nécessitent d'être entre parenthèses.

    Dans les méthodes, on vérifie que toutes les branches atteignables
    possèdent un return du bon type (ou pas de return en cas de void).

    // AUTORISÉ                          // ERREUR
    method int test() {                  method int test() {
    if false { return true; }              if false { return true; }
    else { return false; }                  else {}
    }                                    }

    // AUTORISÉ                         
    method int test() {                  
    if false { return true; } else {}
    return false;                          
    }            

    Pour l'affectation des variables et attributs, les variables doivent être déjà d'éclarés.
    
    On utilise New pour créer des instances de classes.


Extension Traitée: Égalité Structurelle

    Introduction des opérateurs === et =/= pour comparer des objets de manière structurelle.
    Dans le kawalexer, on ajout les tokens EQS (===) et NEQS (=/=).
    Dans le kawaparser, on intégre les opérateurs d'égalité structurelle dans la grammaire. 
    Dans le typechecker, on valide les types des opérandes pour les opérateurs === et =/=, on ne peut pas comparer de des types différents entre eux.
    Dans le interpreter, on evalue les opérateurs d'égalité structurelle lors de l'exécution des expressions. ( = et <>)
    

Difficultés Rencontrées

    Première difficultés rencontrées lors de l'utilisation de "-", car dans l'exemple "2 - 1" le test va bien le passer, 
    mais si on met pas d'espaces entre l'opérand et les variables il va etre reconnu comme opérand unaire et pas binaire.
    
    
    La deuxième et plus grande difficulté je l'ai trouvé lors du codage de la partie de méthodes, avec la méthode eval_call.
    Au début j'ai pensé a tout redéfinir (eval et exec) dans la méthode eval_call, afin de pouvoir séparer les environnements locales et globales.
    Cepandant, il fallait rajouter le double de code, et ca pouvé finir en boucoup d'erreurs bêtes, donc j'ai décidé d'éliminer eval_call.
    J'ai implémenté dans mon eval de base NewCstr, MethCall et This.
    L'élimination de la méthode eval_call a permis de simplifier et de renforcer la structure de l'interpréteur pour le langage . 
    En intégrant directement la gestion des appels de méthodes au sein de la méthode eval, le projet bénéficie d'une meilleure cohésion. 
    Cette décision a contribué à une implémentation plus propre et plus efficace de l'interpréteur, tout en respectant les objectifs du projet.

