# ft_turing

## Pseudo-code

```
initialiser la bande avec des données d'entrée et un programme
positionner la tête de lecture/écriture au début du programme
tant que l'état actuel n'est pas l'état final:
    lire le symbole sous la tête de lecture/écriture
    si des règles correspondent à l'état actuel et au symbole lu:
        écrire un nouveau symbole à la position actuelle (selon la règle)
        déplacer la tête de lecture/écriture vers la gauche ou la droite (selon la règle)
        changer vers un nouvel état (selon la règle)
    sinon:
        arrêter le programme (aucune règle applicable)
```

## Justification RUST

Rust est un langage de programmation qui se concentre principalement sur la sûreté, la vitesse et la concurrence. Bien qu'il ne soit pas purement fonctionnel, Rust supporte les paradigmes de programmation fonctionnelle. Pour justifier votre choix de Rust pour un projet nécessitant l'utilisation d'un langage de programmation fonctionnel, vous pouvez vous appuyer sur plusieurs aspects :

Paradigme de programmation fonctionnelle dans Rust : Rust incorpore des éléments de la programmation fonctionnelle tels que les expressions lambda (closures), les fonctions d'ordre supérieur, les immutabilités par défaut et les patterns matching. Ces caractéristiques vous permettent d'adopter un style de programmation fonctionnelle.

Immutabilité par défaut : Rust encourage l'immutabilité par défaut, ce qui est une caractéristique clé de la programmation fonctionnelle. L'immutabilité conduit à des programmes plus sûrs et plus faciles à comprendre, car elle empêche les modifications d'état inattendues.

Gestion des effets secondaires : La gestion explicite des erreurs via le type Result et la manipulation des valeurs potentiellement absentes via le type Option reflètent l'approche fonctionnelle de la gestion des effets secondaires. Cela permet d'écrire des codes plus prévisibles et plus sûrs, en alignement avec les principes de la programmation fonctionnelle.

Expressivité et concision : La syntaxe de Rust permet d'écrire des abstractions puissantes de manière concise, ce qui est souvent recherché dans la programmation fonctionnelle. Les traits et les types associés offrent un moyen puissant de définir et d'utiliser des abstractions génériques.

Performance : L'un des avantages majeurs de Rust est sa capacité à offrir des performances proches de celles du C/C++, tout en garantissant une sécurité de la mémoire sans garbage collector. Ceci est particulièrement avantageux dans les contextes où les performances sont critiques, mais où vous souhaitez toujours suivre les principes de la programmation fonctionnelle.

Écosystème et communauté : Rust possède une communauté active et un écosystème en croissance, avec une abondance de bibliothèques et d'outils supportant la programmation fonctionnelle.

Pour respecter la consigne de votre projet, il est crucial de vous concentrer sur l'utilisation des aspects fonctionnels de Rust dans votre code. Assurez-vous d'exploiter les caractéristiques mentionnées ci-dessus pour adopter un style de programmation fonctionnelle tout au long de votre projet. En mettant l'accent sur l'immutabilité, en utilisant des fonctions d'ordre supérieur, et en minimisant les effets secondaires, vous pouvez démontrer que Rust, même s'il n'est pas traditionnellement considéré comme un langage fonctionnel pur, est capable de respecter le paradigme fonctionnel pour votre projet.


## Interdiction:

Pour respecter pleinement le paradigme fonctionnel en Rust, il y a certaines pratiques et fonctionnalités du langage que vous devriez éviter, car elles s'éloignent des principes de la programmation fonctionnelle. Voici une liste non exhaustive de ces pratiques et fonctionnalités :

Mutabilité : La programmation fonctionnelle privilégie l'immutabilité. En Rust, cela signifie éviter l'utilisation du mot-clé mut pour déclarer des variables mutables.

Instructions impératives : La programmation fonctionnelle favorise l'expression de la logique à travers des fonctions plutôt que des séquences d'instructions impératives. Cela inclut l'utilisation minimale des boucles for et while au profit des méthodes d'itération de haut niveau et des expressions récursives.

Effets secondaires : Les fonctions pures, qui sont au cœur de la programmation fonctionnelle, doivent être sans effet secondaire. Cela signifie éviter l'utilisation de fonctions ou de blocs de code qui modifient des états globaux, réalisent des entrées/sorties (I/O) ou lancent des exceptions de manière non contrôlée.

Utilisation directe de pointeurs : Bien que Rust sécurise l'utilisation des pointeurs via les types de pointeurs intelligents, la manipulation directe de la mémoire via des pointeurs bruts (*const T, *mut T) s'écarte du paradigme fonctionnel.

Programmation orientée objet (POO) traditionnelle : Bien que Rust permette certains aspects de la POO, comme l'utilisation de traits et de types implémentant ces traits, l'accent mis sur l'héritage et le polymorphisme dynamique à travers les pointeurs de type Box<dyn Trait> ou l'usage intensif de structures de données mutables et d'encapsulation peut s'opposer aux principes de la programmation fonctionnelle.

Variables globales : L'utilisation de variables globales mutables via des constructions comme static mut ou l'utilisation abusive de conteneurs de type singleton qui contiennent un état mutable.

Pour suivre les principes de la programmation fonctionnelle en Rust, concentrez-vous sur l'écriture de fonctions pures, l'utilisation de types immuables, l'adoption de patterns fonctionnels pour la manipulation de données (comme map, filter, fold) et la gestion explicite des erreurs et des valeurs optionnelles à travers Result et Option, plutôt que de recourir aux exceptions ou aux panics. En respectant ces principes, vous pourrez profiter des avantages de la programmation fonctionnelle tout en exploitant la puissance et la performance de Rust.

## Definition:

### Fonction pur :

- Dépendance exclusive sur les arguments passés : Le résultat de la fonction doit uniquement dépendre des valeurs des arguments.
- Pas de modification d'état : La fonction ne doit pas modifier d'états extérieurs (variables globales, entrées/sorties, etc.).
- Pas d'effets de bords : La fonction ne doit avoir aucun effet de bord (pas de logs, pas d'appels réseau, pas d'interaction avec des fichiers, etc.).
- Idempotence : Appeler la fonction plusieurs fois avec les mêmes arguments donnera toujours le même résultat.

### Fonctions anonymes ou lambdas/closures :

Ce sont des fonctions qui sont définies sans être nommées. Elles sont particulièrement utiles pour des opérations courtes que l'on souhaite passer en tant qu'arguments à d'autres fonctions,

Exemple:
`let exemple = |x: i32| x * x;`

### Les Iterator :


