# Instructions 

Consider the grammar below specified using EBNF notation.  

```
expression = expression ( ´+´  | ´-´ ) term | term  
term       = term ( ´*´ | ´/´ ) factor | factor 
factor     = identifier | literal 
identifier = ´a´ | ´b´ | ´c´ | ´d´ | ´e´ | ´f´ | ´g´ | ´h´ | ´i´ | ´j´ | ´k´ | ´l´ | ´m´  
          | ´n´ | ´o´ | ´p´ | ´q´ | ´r´ | ´s´ | ´t´ | ´u´ | ´v´ | ´w´ | ´x´ | ´y´ | ´z´   
literal    = digit { digit } 
digit      = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
```

Write a lexical analyzer for the language described by the grammar. The output of your lexical analyzer should be a list of pairs containing a lexical unit followed by its token number, in the order of their appearance.  Below are some source codes (with expected outputs) for you to try.  

## Source 1 

```
52 + x * 231 / y - 8 
```

output: 

```
Lexeme(52, LITERAL)
Lexeme(+, ADDITION)
Lexeme(x, IDENTIFIER)
Lexeme(*, MULTIPLICATION)
Lexeme(231, LITERAL)
Lexeme(/, DIVISION)
Lexeme(y, IDENTIFIER)
Lexeme(-, SUBTRACTION)
Lexeme(8, LITERAL)
```
 
## Source 2 

```
z + 3 - x * 2931 
```
 
output: 

```
Lexeme(z, IDENTIFIER)
Lexeme(+, ADDITION)
Lexeme(3, LITERAL)
Lexeme(-, SUBTRACTION)
Lexeme(x, IDENTIFIER)
Lexeme(*, MULTIPLICATION)
Lexeme(2931, LITERAL)
``` 

## Source 3 

Note that this source has two tabs after “+”, new lines, and multiple letters together. 

```
x +   4 

 

- 2 / abc + 2 
```

output: 

```
Lexeme(x, IDENTIFIER)
Lexeme(+, ADDITION)
Lexeme(4, LITERAL)
Lexeme(-, SUBTRACTION)
Lexeme(2, LITERAL)
Lexeme(/, DIVISION)
Lexeme(a, IDENTIFIER)
Lexeme(b, IDENTIFIER)
Lexeme(c, IDENTIFIER)
Lexeme(+, ADDITION)
Lexeme(2, LITERAL)
```
 
## Source 4

```
x > 4 * c 
```

output: 

```
Exception: Lexical Analyzer Error: unrecognizable symbol found! 
```

# Update

Update your lexical analyzer by changing the way identifiers are defined using the following: 

```
identifier = letter { ( letter | digit ) } 
```