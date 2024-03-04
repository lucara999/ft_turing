
# 1. Utiliser des fonctions pures
Une fonction pure est une fonction dont le résultat dépend uniquement de ses arguments et qui ne produit aucun effet de bord (comme la modification d'une variable extérieure ou l'affichage à l'écran). Pour cela, chaque fois que vous écrivez une fonction, assurez-vous qu'elle respecte ces critères.

```
def add(a, b):
	return a + b
```

# 2. Immutabilité
Évitez de modifier des variables ou des objets. Une fois qu'une structure de données est créée, elle ne doit pas être changée. Si vous devez modifier un objet, vous devez en créer un nouveau qui représente l'état modifié.

```
def add_element_immutable(seq, elem):
    return seq + (elem,)  # Crée un nouveau tuple au lieu de modifier l'original
```

# 3. Utiliser des expressions plutôt que des instructions
Privilégiez l'utilisation d'expressions qui retournent une valeur plutôt que des instructions qui effectuent des actions. Cela signifie favoriser if comme expression plutôt que comme bloc de contrôle, et utiliser des fonctions comme map, filter, et reduce pour opérer sur des collections.

```
result = map(lambda x: x * 2, [1, 2, 3])  # Double chaque élément
```

# 4. Fonctions d'ordre supérieur
Utilisez des fonctions qui prennent d'autres fonctions en arguments ou qui retournent des fonctions. Cela permet de créer des abstractions puissantes et réutilisables.

```
def apply_function(f, value):
    return f(value)

def multiply_by_2(x):
    return x * 2

result = apply_function(multiply_by_2, 5)  # Résultat : 10
```

# 5. Recursion
Préférez la récursion aux boucles classiques. Cela peut nécessiter de repenser la manière dont vous structurez certains algorithmes, mais c'est plus en ligne avec la programmation fonctionnelle.

```
def factorial(n):
    if n == 0:
        return 1
    else:
        return n * factorial(n-1)
```

# 6. Éviter les effets de bord
Assurez-vous que vos fonctions n'ont pas d'effets de bord, comme modifier un état global, écrire dans un fichier ou dans une base de données. Tout changement d'état doit être fait de manière contrôlée et explicite.

Utiliser des bibliothèques fonctionnelles
Il existe plusieurs bibliothèques qui peuvent vous aider à adopter un style plus fonctionnel en Python, comme functools, itertools, et des bibliothèques tierces comme toolz ou fn.py. Ces bibliothèques offrent des utilitaires pour faciliter la programmation fonctionnelle.

```
from functools import reduce
```

# Somme des éléments d'une liste en utilisant reduce
```
sum = reduce(lambda x, y: x + y, [1, 2, 3, 4, 5])
```
En suivant ces principes et en pratiquant régulièrement, vous pourrez écrire du code Python de manière purement fonctionnelle. Cela peut nécessiter un changement de mentalité si vous êtes habitué à la programmation impérative ou orientée objet, mais c'est une compétence précieuse qui peut conduire à un code plus clair, plus testable et souvent plus efficace.


# Les Interdiction

## 1. Mutabilité
La mutabilité est couramment utilisée dans les styles de programmation impératif et orienté objet, mais elle va à l'encontre des principes de la programmation fonctionnelle. Cela inclut :

Modification des éléments d'une liste.
Modification des champs d'une instance d'objet.
Utilisation de types de données mutables comme les listes ou les dictionnaires pour des opérations qui modifient leur état.

## 2. Boucles for et while
Les boucles sont un élément de base de la programmation impérative pour itérer sur des collections de données. En programmation fonctionnelle, on préfère utiliser des fonctions comme map, filter, et reduce pour travailler avec des collections de manière déclarative.

## 3. Instructions if pour le contrôle de flux
Bien que les instructions if soient omniprésentes dans tous les styles de programmation, en programmation fonctionnelle, on tend à les utiliser sous forme d'expressions pour retourner des valeurs plutôt que pour contrôler le flux du programme.

## 4. Variables et états globaux
L'utilisation de variables globales ou d'états partagés peut entraîner des effets de bord indésirables et rendre le code moins prévisible. En programmation fonctionnelle, on évite ces pratiques en passant explicitement tout ce dont une fonction a besoin en tant qu'arguments.

## 5. Effets de bord
Les effets de bord, comme la modification d'un état externe, l'écriture dans un fichier ou une base de données, ou la modification d'une variable globale, sont à éviter. Les fonctions doivent être pures, c'est-à-dire que pour des entrées données, elles doivent toujours produire le même résultat sans modifier un état externe.

## 6. Utilisation de classes et d'objets mutables
Bien que la programmation orientée objet soit un pilier de Python, l'utilisation de classes et d'instances d'objets avec des états internes mutables est contraire aux principes de la programmation fonctionnelle. Si des objets sont utilisés, ils devraient être immuables.

## 7. Fonctions avec plusieurs points de sortie
Les fonctions avec plusieurs points de sortie (plusieurs return à différents endroits) peuvent rendre le suivi du flux logique plus difficile. En programmation fonctionnelle, on tend à concevoir des fonctions avec un seul point de sortie pour rendre leur comportement plus prévisible.

Alternatives et Solutions
Pour chaque élément non autorisé mentionné, il existe des alternatives fonctionnelles :

Mutabilité : Utiliser des structures de données immuables (comme les tuples) et des fonctions qui retournent de nouveaux états plutôt que de modifier l'état existant.
Boucles : Utiliser des fonctions d'ordre supérieur comme map, filter, reduce, et des compréhensions de liste pour les opérations sur les collections.
Instructions if : Utiliser l'expression ternaire pour retourner des valeurs basées sur des conditions.
Variables et états globaux : Passer tous les états nécessaires en tant qu'arguments aux fonctions.
Effets de bord : Isoler les effets de bord et utiliser des fonctions pures autant que possible.
Classes et objets mutables : Privilégier l'utilisation de collections.namedtuple ou de classes avec des champs immuables pour représenter des données.
En suivant ces directives et en évitant ces pratiques, vous serez en mesure de concevoir des programmes en Python qui adhèrent plus étroitement aux principes de la programmation fonctionnelle.
