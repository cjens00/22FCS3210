# Instructions

Consider the grammar below specified using EBNF notation. Note that this grammar is almost the same as the one used in Activity 04. The difference is that a factor can be an expression in parentheses. 

```
expression = expression ( ´+´  | ´-´ ) term | term  
term       = term ( ´*´ | ´/´ ) factor | factor 
factor     = identifier | literal | ´(´ expression ´)´ 
identifier = letter { ( letter | digit ) } 
letter     = ´a´ | ´b´ | ´c´ | ´d´ | ´e´ | ´f´ | ´g´ | ´h´ | ´i´ | ´j´ | ´k´ | ´l´ | ´m´ | ´n´ | ´o´ | ´p´ | ´q´ | ´r´ | ´s´ | ´t´ | ´u´ | ´v´ | ´w´ | ´x´ | ´y´ | ´z´   
literal    = digit { digit } 
digit      = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´   
```

Rewrite the grammar above removing all left-recursive productions. Then write a recursive-descent parser for the language described by the grammar. The output of your parser is a parse tree describing the syntax structure of the input code. 