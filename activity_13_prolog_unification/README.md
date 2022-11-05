# Instructions

Consider the following knowledge base: 

```
elf(bing). 
elf(penny). 
elf('Gabriel'). 
elf(trixie). 
wizard(merlin). 
wizard(gandalf). 
witch(jadis). 
magic(X) :- elf(X). 
magic(X) :- wizard(X). 
magic(X) :- witch(X). 
freeze(X,Y) :- wizard(X), elf(Y). 
```

Which of the following queries are satisfied?  Where relevant, give all the variable instantiations that lead to success. 

```
elf(gabriel).  

wizard('Merlin').  

wizard(X).  

magic(X).   

witch(Jadis).  

freeze(merlin, Y).  

freeze(X, merlin).   
```