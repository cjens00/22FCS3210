# BNF Notation

Consider the grammar below written in extended BNF (Backus-Naur Form) to formally define the syntax of an IF function in a hypothetical spreadsheet software.   

```
IF = ´IF´ ´(´ BL ´;´ AR ´;´ AR ´)´ 
BL = CL ( ´>´ | ´>=´ | ´<´ | ´<=´ | ´==´ | ´<>´ ) CL 
AR = CL | CL ( ´+´ | ´-´ | ´*´ | ´/´ ) CL | IF 
CL = [ ´$´ ] LT [ ´$´ ] DG { DG } 
LT = ´A´ | ´B´ | ´C´ | ´D´ | ´E´ 
DG = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´ 
```

## Q1

Say yes/no whether each word can be generated from the grammar above.  

### a) 

```
IF ($A <> B2 ; A2 ; $B3) 
```

NO!

### b)  

```
IF (IF (B2 <> B1 ; E5 ; D2) ; A2 ; B3) 
```

NO!

### c)

```
IF ($A5 <> B2 ; A2 ; B3) 
```
IF -> ´IF´ ´(´ BL ´;´ AR ´;´ AR ´)´ ->
´IF´ ´(´ CL ´<>´ CL  ´;´ AR ´;´ AR ´)´ ->
´IF´ ´(´ ´$´ LT DG ´<>´ CL  ´;´ AR ´;´ AR ´)´ ->
´IF´ ´(´ ´$´ ´A´ DG ´<>´ CL  ´;´ AR ´;´ AR ´)´ ->
´IF´ ´(´ ´$´ ´A´ 5 ´<>´ CL  ´;´ AR ´;´ AR ´)´ ->
´IF´ ´(´ ´$´ ´A´ 5 ´<>´ LT DG ´;´ AR ´;´ AR ´)´ ->
´IF´ ´(´ ´$´ ´A´ 5 ´<>´ B DG ´;´ AR ´;´ AR ´)´ ->
´IF´ ´(´ ´$´ ´A´ 5 ´<>´ B 2 ´;´ AR ´;´ AR ´)´ ->
´IF´ ´(´ ´$´ ´A´ 5 ´<>´ B 2 ´;´ CL ´;´ AR ´)´ ->
´IF´ ´(´ ´$´ ´A´ 5 ´<>´ B 2 ´;´ LT DG ´;´ AR ´)´ ->
´IF´ ´(´ ´$´ ´A´ 5 ´<>´ B 2 ´;´ ´A´ DG ´;´ AR ´)´ ->
´IF´ ´(´ ´$´ ´A´ 5 ´<>´ B 2 ´;´ ´A´ 2 ´;´ AR ´)´ ->
´IF´ ´(´ ´$´ ´A´ 5 ´<>´ B 2 ´;´ ´A´ 2 ´;´ LT DG ´)´ ->
´IF´ ´(´ ´$´ ´A´ 5 ´<>´ B 2 ´;´ ´A´ 2 ´;´ ´B´ DG ´)´ ->
´IF´ ´(´ ´$´ ´A´ 5 ´<>´ B 2 ´;´ ´A´ 2 ´;´ ´B´ 3 ´)´

### d)

```
IF ($A21 <> B3 ; A2 ; IF (B2 <> B1 ; E5 ; D2)) 
```

YES!
 
## Q2

Show the sequence of productions to generate the word below. 

```
IF ( A$1 > $B3 ; A2 ; A3 ) 
```
 
## Q3

Rewrite the given grammar using the formal notation below.  

G = (V, T, P, S), where:  

* V is a finite set of variables 
* T is a finite set of terminal symbols 
* P is a finite set of production rules 
* S is the start variable 

Make sure that P has one production per line and no extended BNF notation; you can stop when you reach productions LT and DG. 

```
IF = ´IF´ ´(´ BL ´;´ AR ´;´ AR ´)´ 
BL = CL ´>´ CL 
BL = CL ´>=´ CL
BL = CL ´<´ CL
BL = CL ´<=´ CL
BL = CL ´==´ CL
BL = CL ´<>>´ CL
AR = CL 
AR = CL ´+´ CL 
AR = CL ´-´ CL 
AR = CL ´*´ CL 
AR = CL ´/´ CL 
AR = IF
CL = LT DG' 
CL = ´$´ LT DG' 
CL = LT ´$´ DG' 
CL = ´$´ LT ´$´ DG' 
DG' = DG DG' | DG 
LT = ´A´ | ´B´ | ´C´ | ´D´ | ´E´ 
DG = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´ 
```

## Q4 

Rewrite the grammar below in a more compact way using EBNF. 

```
expr   = ´-´ num | num 
num    = digits | digits ´.´ digits 
digits = digit | digit digits 
digit  = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´ 
``` 

Solution:
```
expr   = [ ´-´ ] num 
num    = digits [ ´.´ digits ]
digits = digit { digit }
digit  = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´ 
``` 

