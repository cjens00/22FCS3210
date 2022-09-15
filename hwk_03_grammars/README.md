# Formal Grammars

For each of the grammars from Q1-3, classify them as either regular or context-free. Then answer yes/no whether each word can be generated from the grammar. If you answer yes, show the sequence of productions to generate the word.  When you are done, submit this README.md file with your answers on Canvas. 

## Q1

```
G = (V, T, P, S) 
V = {S, NP, NOM, VP, Det, Noun, Verb, Aux}
T = {that, this, a, the, man, book, flight, meal, include, read, does}
P = {
    S → NP VP 
    S → Aux NP VP 
    S → VP 
    NP → Det NOM 
    NOM → Noun
    NOM → Noun NOM
    VP → Verb
    VP → Verb NP
    Det → that | this | a | the
    Noun → book | flight | meal | man
    Verb → book | include | read
    Aux → does
} 
```
 
### a)

```
does a meal include a book 
```

### b) 

```
this flight book a meal
``` 

### c) 

```
a man read a book
```

## Q2

```
G = ({S, A, B}, {a, b}, P, S) 
P = {S → aA, A → bB | ε, B → aA}    
```

### a) 

```
a
```

### b) 

```
aba
```

### c) 

```
bb
```

### d) 

```
abb
```

## Q3

```
G = ({S, A}, {0, 1}, P, S) 
P = {S → A0, A→A01|ε}           
```
 
### a) 

```
00
```
 
### b) 

```
ε
```

### c)

```
010
```
 
### d) 

```
0101010
```
 