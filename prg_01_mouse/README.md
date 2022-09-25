# Introduction

The Mouse PL was first described by [Peter Gronono](http://www.retroprogramming.com/2012/08/mouse-language-for-microcomputers-by.html) in 1979. Being a stack-based PL, Mouse simplifies implementation, making it an appropriate choice for this assignment. This section introduces (a version of) Mouse's syntax and semantics. 

```
mouse         = { line } ´$$´ 
line          = statement | comment
statement     = ´?´ | ´!´ | string | identifier | ´=´ | literal | ´+´ | ´-´ | ´*´ | ´/´ | ´%´ | ´<´ | ´<=´ | ´>´ | ´>=´ | ´==´ | ´!=´ | ´^´ | ´.´ |  | if | while
comment       = ´;´ { character } ´\n´
string        = ´"´ { character } ´"´ 
identifier    = letter [ { letter | digit | ´_´ } ]
literal       = ´0´ | nonzero { digit } 
nonzero       = ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´ 
digit         = ´0´ | nonzero 
if            = ´[´ { line } ´]´ 
while         = ´(´ { line } ´)´ 
letter        = ´a´ | ´b´ | ´c´ | ´d´ | ´e´ | ´f´ | ´g´ | ´h´ | ´i´ | ´j´ | ´k´ | ´l´ | ´m´ | ´n´ | ´o´ | ´p´ | ´q´ | ´r´ | ´s´ | ´t´ | ´u´ | ´v´ | ´x´ | ´y´ | ´w´ | ´z´ | ´A´ | ´B´ | ´C´ | ´D´ | ´E´ | ´F´ | ´G´ | ´H´ | ´I´ | ´J´ | ´K´ | ´L´ | ´M´ | ´N´ | ´O´ | ´P´ | ´Q´ | ´R´ | ´S´ | ´T´ | ´U´ | ´V´ | ´X´ | ´Y´ | ´W´ | ´Z´ 
character     = letter | digit | ´ ´ | ´,´ | ´;´ | ´-´ | ´:´ | ´'´ | ´(´ | ´)´ | ´[´ | ´]´
```


As described by Mouse's EBNF above, a Mouse program is made of lines (comments or statements) followed by an end-of-program symbol ($$). A comment begins with a semicolon and must end with a new line. The semantics in Mouse is better understood by how they affect memory and a stack.  

| Statement  | Semantics                                                                                                                               |
|------------|-----------------------------------------------------------------------------------------------------------------------------------------|
| ?          | reads a number from the (standard) input and pushes it onto the stack                                                                   |
| !          | pops the stack, displaying the result on the (standard) output                                                                          |
| string     | displays the string on the (standard) output (no effect on the stack)                                                                   |
| identifier | pushes the address of the identifier (variable) onto the stack                                                                          |
| =          | pops the stack twice, let's say b and a (in this order), assigning b to the variable pointed by a                                       |
| literal    | pushes the literal onto the stack                                                                                                       |
| +          | pops the stack twice, let's say b and a (in this order), then pushes a + b onto the stack                                               |
| -          | pops the stack twice, let's say b and a (in this order), then pushes a - b onto the stack                                               |
| *          | pops the stack twice, let's say b and a (in this order), then pushes a * b onto the stack                                               |
| /          | pops the stack twice, let's say b and a (in this order), then pushes a / b (integer division) onto the stack                            |
| %          | pops the stack twice, let's say b and a (in this order), then pushes a % b (the remain of the division) onto the stack2                 |
| <          | pops the stack twice, let's say b and a (in this order), then pushes 1 if a < b (0 otherwise)                                           |     
| <=         | pops the stack twice, let's say b and a (in this order), then pushes 1 if a <= b (0 otherwise)                                          |   
| >          | pops the stack twice, let's say b and a (in this order), then pushes 1 if a > b (0 otherwise)                                           |   
| >=         | pops the stack twice, let's say b and a (in this order), then pushes 1 if a >= b (0 otherwise)                                          |   
| ==         | pops the stack twice, let's say b and a (in this order), then pushes 1 if a == b (0 otherwise)                                          |   
| !=         | pops the stack twice, let's say b and a (in this order), then pushes 1 if a != b (0 otherwise)                                          |   
| ^          | pops the stack and breaks out of a loop if the value returned is different than zero                                                    |
| .          | pops the stack and uses its value as the address of a variable to be retrieved; the value of the variable is then pushed onto the stack |
| [ ]        | pops the stack and executes the statement(s) inside the brackets only if the value returned is greater than zero                        |
| ( )        | executes the statement(s) inside the parentheses continuously                                         |

The goal of this assignment is the implementation of a lexical analyzer, a syntax analyzer, and an interpreter for the Mouse PL as described here.  For that, you are required to code your solution using Scala.  You are not obligated to use the code that was provided. However, your solution must be modular and use a similar interface. Therefore, I should be able to use my syntax analyzer with your lexical analyzer, or run my mouse interpreter with your syntax analyzer. 

# Tokens

The tokens that you should use in this project are already identified below: 

* EOF (end-of-file)
* EO_PRG (end-of-program) $$
* COMMENT (comments begin with ; and end with a new line)
* INPUT ?
* OUTPUT !
* STRING (strings are enclosed in double quotes)
* IDENTIFIER (begin with a letter and are followed by letters, digits, or underline)
* ASSIGNMENT =
* LITERAL (integer literals only)
* ADDITION +
* SUBTRACTION -
* MULTIPLICATION *
* DIVISION (integer division only) /
* MODULUS %
* LESS <
* LESS_EQUAL <=
* GREATER >
* GREATER_EQUAL >=
* EQUAL ==
* DIFFERENT !=
* BREAK (break out of a loop) ^
* DOT .
* OPEN_PAR (
* CLOSE_PAR )
* OPEN_BRACKET [
* CLOSE_BRACKET ]

# The Lexical Analyzer

In the first part of this assignment you are asked to implement a lexical analyzer that is able to identify all tokens in a given Mouse source program. This part of the assignment is worth 25 points (1 point for each token correctly identified). Your program should also flag unrecognized symbols by throwing an exception. 

The lexical analyzer must implement the Iterable[Lexeme] trait.

# The Syntax Analyzer

Next, you are asked to implement a top-down recursive descent parser for the Mouse PL. Your parse tree should be based on the provided grammar. See the examples in the next section to verify how each parse tree should look like. If the generated parse tree is different, the mouse interpreter will not be able to run the examples. The syntax analyzer is worth 50 points (10 points for each parse method implemented). The syntax analyzer must have a public method called parse that returns the parse tree (based on the given Node class). 

In case the syntax analyzer reaches an impossible state because of a syntax error, it must report it by throwing an appropriate error message. For example: Syntax Analyzer Error: ] expected!

## Examples 

The examples below illustrate how to write simple programs using the Mouse PL and how their parse trees should look like. 

### example0.mouse

The first example ([example0.mouse](example0.mouse)) illustrates that all Mouse programs must end with $$.

```
; CS3210 - Principles of Programming Languages - Fall 2022
; Instructor: Thyago Mota
; Description: does nothing program

$$
```

``` 
Lexeme(mouse)
	Lexeme(line)
		Lexeme(CS3210 - Principles of Programming Languages - Fall 2022, COMMENT)
	Lexeme(line)
		Lexeme(Instructor: Thyago Mota, COMMENT)
	Lexeme(line)
		Lexeme(Description: does nothing program, COMMENT)
	Lexeme($$, EO_PRG)
```

### example1.mouse

[example1.mouse](example1.mouse) is how you would write a "Hello World" program in Mouse. 

```
; CS3210 - Principles of Programming Languages - Fall 2022
; Instructor: Thyago Mota
; Description: Hello World

"Hello World!"
$$
```

``` 
Lexeme(mouse)
	Lexeme(line)
		Lexeme(CS3210 - Principles of Programming Languages - Fall 2022, COMMENT)
	Lexeme(line)
		Lexeme(Instructor: Thyago Mota, COMMENT)
	Lexeme(line)
		Lexeme(Description: Hello World, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(Hello World!, STRING)
	Lexeme($$, EO_PRG)
```

### example2.mouse 

[example2.mouse](example2.mouse) performs a simple I/O. 

``` 
; CS3210 - Principles of Programming Languages - Fall 2022
; Instructor: Thyago Mota
; Description: simple input and output

N "N? " ? =    ; reads an integer from the user, saving it in variable N
"N is " N . !  ; displays the value of variable N
$$
```

``` 
Lexeme(mouse)
	Lexeme(line)
		Lexeme(CS3210 - Principles of Programming Languages - Fall 2022, COMMENT)
	Lexeme(line)
		Lexeme(Instructor: Thyago Mota, COMMENT)
	Lexeme(line)
		Lexeme(Description: simple input and output, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(N, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(N? , STRING)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(?, INPUT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(=, ASSIGNMENT)
	Lexeme(line)
		Lexeme(reads an integer from the user, saving it in variable N, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(N is , STRING)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(N, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(!, OUTPUT)
	Lexeme(line)
		Lexeme(displays the value of variable N, COMMENT)
	Lexeme($$, EO_PRG)
```

### example3.mouse 

[example3.mouse](example3.mouse) reads a number and tells if the number is odd or even. 

```
; CS3210 - Principles of Programming Languages - Fall 2022
; Instructor: Thyago Mota
; Description: reads a number and tells if the number is odd or even

N "N? " ? =                   ; reads an integer from the user, saving it in variable N
N .                           ; pushes the value of N onto the stack
N . 2 % [ ! " is odd!" ]      ; pushes the value of N again and checks if it's odd
N . 1 + 2 % [ ! " is even!" ] ; pushes the value of N again and checks if it's even

$$
```

```
Lexeme(mouse)
	Lexeme(line)
		Lexeme(CS3210 - Principles of Programming Languages - Fall 2022, COMMENT)
	Lexeme(line)
		Lexeme(Instructor: Thyago Mota, COMMENT)
	Lexeme(line)
		Lexeme(Description: reads a number and tells if the number is odd or even, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(N, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(N? , STRING)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(?, INPUT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(=, ASSIGNMENT)
	Lexeme(line)
		Lexeme(reads an integer from the user, saving it in variable N, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(N, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(pushes the value of N onto the stack, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(N, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(2, LITERAL)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(%, MODULUS)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(if)
				Lexeme([, OPEN_BRACKET)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(!, OUTPUT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme( is odd!, STRING)
				Lexeme(], CLOSE_BRACKET)
	Lexeme(line)
		Lexeme(pushes the value of N again and checks if its odd, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(N, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(1, LITERAL)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(+, ADDITION)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(2, LITERAL)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(%, MODULUS)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(if)
				Lexeme([, OPEN_BRACKET)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(!, OUTPUT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme( is even!, STRING)
				Lexeme(], CLOSE_BRACKET)
	Lexeme(line)
		Lexeme(pushes the value of N again and checks if its even, COMMENT)
	Lexeme($$, EO_PRG)
```

### example4.mouse

[example4.mouse](example4.mouse) reads two numbers and tells which one is greater than the other.

``` 
; CS3210 - Principles of Programming Languages - Fall 2022
; Instructor: Thyago Mota
; Description: reads two numbers and tells which one is greater than the other

A "A? " ? =  ; reads an integer from the user, saving it in variable A
B "B? " ? =  ; reads an integer from the user, saving it in variable B
A. B. >  [ A. ! " > "  B. ! ]
A. B. <  [ A. ! " < "  B. ! ]
A. B. == [ A. ! " == " B. ! ]

$$
```

``` 
Lexeme(mouse)
	Lexeme(line)
		Lexeme(CS3210 - Principles of Programming Languages - Fall 2022, COMMENT)
	Lexeme(line)
		Lexeme(Instructor: Thyago Mota, COMMENT)
	Lexeme(line)
		Lexeme(Description: reads two numbers and tells which one is greater than the other, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(A, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(A? , STRING)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(?, INPUT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(=, ASSIGNMENT)
	Lexeme(line)
		Lexeme(reads an integer from the user, saving it in variable A, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(B, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(B? , STRING)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(?, INPUT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(=, ASSIGNMENT)
	Lexeme(line)
		Lexeme(reads an integer from the user, saving it in variable B, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(A, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(B, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(>, GREATER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(if)
				Lexeme([, OPEN_BRACKET)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(A, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(!, OUTPUT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme( > , STRING)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(B, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(!, OUTPUT)
				Lexeme(], CLOSE_BRACKET)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(A, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(B, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(<, LESS)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(if)
				Lexeme([, OPEN_BRACKET)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(A, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(!, OUTPUT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme( < , STRING)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(B, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(!, OUTPUT)
				Lexeme(], CLOSE_BRACKET)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(A, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(B, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(==, EQUAL)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(if)
				Lexeme([, OPEN_BRACKET)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(A, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(!, OUTPUT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme( == , STRING)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(B, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(!, OUTPUT)
				Lexeme(], CLOSE_BRACKET)
	Lexeme($$, EO_PRG)
```

### example5.mouse

[example5.mouse](example5.mouse) reads three numbers and tells which is one is the greatest.

``` 
; CS3210 - Principles of Programming Languages - Fall 2022
; Instructor: Thyago Mota
; Description: reads three numbers and tells which is one is the greatest

A "A? " ? =
B "B? " ? =
C "C? " ? =

; is A the greatest?
A. B. >=  [
    A. C. >= [ A. ! " is the greatest!" ]
]

; is B the greatest?
B. A. >=  [
    B. C. >= [ B. ! " is the greatest!" ]
]

; is C the greatest?
C. A. >=  [
    C. B. >= [ B. ! " is the greatest!" ]
]

$$
```

``` 
Lexeme(mouse)
	Lexeme(line)
		Lexeme(CS3210 - Principles of Programming Languages - Fall 2022, COMMENT)
	Lexeme(line)
		Lexeme(Instructor: Thyago Mota, COMMENT)
	Lexeme(line)
		Lexeme(Description: reads three numbers and tells which is one is the greatest, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(A, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(A? , STRING)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(?, INPUT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(=, ASSIGNMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(B, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(B? , STRING)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(?, INPUT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(=, ASSIGNMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(C, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(C? , STRING)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(?, INPUT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(=, ASSIGNMENT)
	Lexeme(line)
		Lexeme(is A the greatest?, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(A, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(B, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(>=, GREATER_EQUAL)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(if)
				Lexeme([, OPEN_BRACKET)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(A, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(C, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(>=, GREATER_EQUAL)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(if)
							Lexeme([, OPEN_BRACKET)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(A, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(., DOT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(!, OUTPUT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme( is the greatest!, STRING)
							Lexeme(], CLOSE_BRACKET)
				Lexeme(], CLOSE_BRACKET)
	Lexeme(line)
		Lexeme(is B the greatest?, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(B, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(A, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(>=, GREATER_EQUAL)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(if)
				Lexeme([, OPEN_BRACKET)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(B, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(C, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(>=, GREATER_EQUAL)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(if)
							Lexeme([, OPEN_BRACKET)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(B, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(., DOT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(!, OUTPUT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme( is the greatest!, STRING)
							Lexeme(], CLOSE_BRACKET)
				Lexeme(], CLOSE_BRACKET)
	Lexeme(line)
		Lexeme(is C the greatest?, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(C, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(A, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(>=, GREATER_EQUAL)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(if)
				Lexeme([, OPEN_BRACKET)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(C, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(B, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(>=, GREATER_EQUAL)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(if)
							Lexeme([, OPEN_BRACKET)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(B, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(., DOT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(!, OUTPUT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme( is the greatest!, STRING)
							Lexeme(], CLOSE_BRACKET)
				Lexeme(], CLOSE_BRACKET)
	Lexeme($$, EO_PRG)
```

### example6.mouse

[example6.mouse](example6.mouse) performs a simple count from 1 to N. It illustrates how to perform simple input validations and how loops work in Mouse. 

```
; CS3210 - Principles of Programming Languages - Fall 2022
; Instructor: Thyago Mota
; Description: a simple count from 1 to N

N "N? " ? =       ; reads the value of N

N. 1 < [ "N has to be at least 1!" ]
N. 1 >= [
    i 1 =             ; initialize counter variable
    (
        i. N. > ^     ; checks if counter reached N
        i. ! " "      ; displays counter
        i i. 1 + =    ; increments counter
    )
]

$$
```

``` 
Lexeme(mouse)
	Lexeme(line)
		Lexeme(CS3210 - Principles of Programming Languages - Fall 2022, COMMENT)
	Lexeme(line)
		Lexeme(Instructor: Thyago Mota, COMMENT)
	Lexeme(line)
		Lexeme(Description: a simple count from 1 to N, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(N, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(N? , STRING)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(?, INPUT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(=, ASSIGNMENT)
	Lexeme(line)
		Lexeme(reads the value of N, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(N, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(1, LITERAL)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(<, LESS)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(if)
				Lexeme([, OPEN_BRACKET)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(N has to be at least 1!, STRING)
				Lexeme(], CLOSE_BRACKET)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(N, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(1, LITERAL)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(>=, GREATER_EQUAL)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(if)
				Lexeme([, OPEN_BRACKET)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(i, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(1, LITERAL)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(=, ASSIGNMENT)
				Lexeme(line)
					Lexeme(initialize counter variable, COMMENT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(while)
							Lexeme((, OPEN_PAR)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(i, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(., DOT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(N, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(., DOT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(>, GREATER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(^, BREAK)
							Lexeme(line)
								Lexeme(checks if counter reached N, COMMENT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(i, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(., DOT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(!, OUTPUT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme( , STRING)
							Lexeme(line)
								Lexeme(displays counter, COMMENT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(i, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(i, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(., DOT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(1, LITERAL)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(+, ADDITION)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(=, ASSIGNMENT)
							Lexeme(line)
								Lexeme(increments counter, COMMENT)
							Lexeme(), CLOSE_PAR)
				Lexeme(], CLOSE_BRACKET)
	Lexeme($$, EO_PRG)
```

### example7.mouse

[example7.mouse](example7.mouse) reads N and M (both at least 1) and displays all (i, j) combinations, with i in N and j in M. It illustrates how to perform continuous input validations and how to implement multi-level loops in Mouse. 

``` 
; CS3210 - Principles of Programming Languages - Fall 2022
; Instructor: Thyago Mota
; Description: reads N and M (both at least 1) and displays all (i, j) combinations, with i in N and j in M

(
    N "N? " ? =
    N. 1 < [ "N has to be at least 1!" ]
    N. 1 >= ^
)

(
    M "M? " ? =
    M. 1 < [ "M has to be at least 1!" ]
    M. 1 >= ^
)

i 1 =
(
    i. N. > ^
    j 1 =
    (
        j. M. > ^
        "(" i.! "," j.! ") "
        j j. 1 + =
    )
    i i. 1 + =
)

$$
```

```
Lexeme(mouse)
	Lexeme(line)
		Lexeme(CS3210 - Principles of Programming Languages - Fall 2022, COMMENT)
	Lexeme(line)
		Lexeme(Instructor: Thyago Mota, COMMENT)
	Lexeme(line)
		Lexeme(Description: reads N and M (both at least 1) and displays all (i, j) combinations, with i in N and j in M, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(while)
				Lexeme((, OPEN_PAR)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(N, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(N? , STRING)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(?, INPUT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(=, ASSIGNMENT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(N, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(1, LITERAL)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(<, LESS)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(if)
							Lexeme([, OPEN_BRACKET)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(N has to be at least 1!, STRING)
							Lexeme(], CLOSE_BRACKET)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(N, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(1, LITERAL)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(>=, GREATER_EQUAL)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(^, BREAK)
				Lexeme(), CLOSE_PAR)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(while)
				Lexeme((, OPEN_PAR)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(M, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(M? , STRING)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(?, INPUT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(=, ASSIGNMENT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(M, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(1, LITERAL)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(<, LESS)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(if)
							Lexeme([, OPEN_BRACKET)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(M has to be at least 1!, STRING)
							Lexeme(], CLOSE_BRACKET)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(M, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(1, LITERAL)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(>=, GREATER_EQUAL)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(^, BREAK)
				Lexeme(), CLOSE_PAR)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(i, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(1, LITERAL)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(=, ASSIGNMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(while)
				Lexeme((, OPEN_PAR)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(i, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(N, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(>, GREATER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(^, BREAK)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(j, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(1, LITERAL)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(=, ASSIGNMENT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(while)
							Lexeme((, OPEN_PAR)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(j, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(., DOT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(M, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(., DOT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(>, GREATER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(^, BREAK)
							Lexeme(line)
								Lexeme(statement)
									Lexeme((, STRING)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(i, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(., DOT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(!, OUTPUT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(,, STRING)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(j, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(., DOT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(!, OUTPUT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme() , STRING)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(j, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(j, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(., DOT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(1, LITERAL)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(+, ADDITION)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(=, ASSIGNMENT)
							Lexeme(), CLOSE_PAR)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(i, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(i, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(1, LITERAL)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(+, ADDITION)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(=, ASSIGNMENT)
				Lexeme(), CLOSE_PAR)
	Lexeme($$, EO_PRG) 
```

### example8.mouse

[example8.mouse](example8.mouse) reads N (at least 0) and displays the factorial of N. 

``` 
; CS3210 - Principles of Programming Languages - Fall 2022
; Instructor: Thyago Mota
; Description: reads N (at least 0) and displays N!

(
    N "N? " ? =
    N. 0 < [ "N has to be at least 0!" ]
    N. 0 >= ^
)

; special case when N=0
N. 0 == [ "0! = 1" ]

; all other cases
N. 0 > [
    i 2 =
    F 1 =
    (
        i. N. > ^
        F F. i. * =
        i i. 1 + =
    )
    N.! "! = " F.!
]

$$
```

```
Lexeme(mouse)
	Lexeme(line)
		Lexeme(CS3210 - Principles of Programming Languages - Fall 2022, COMMENT)
	Lexeme(line)
		Lexeme(Instructor: Thyago Mota, COMMENT)
	Lexeme(line)
		Lexeme(Description: reads N (at least 0) and displays N!, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(while)
				Lexeme((, OPEN_PAR)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(N, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(N? , STRING)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(?, INPUT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(=, ASSIGNMENT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(N, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(0, LITERAL)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(<, LESS)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(if)
							Lexeme([, OPEN_BRACKET)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(N has to be at least 0!, STRING)
							Lexeme(], CLOSE_BRACKET)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(N, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(0, LITERAL)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(>=, GREATER_EQUAL)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(^, BREAK)
				Lexeme(), CLOSE_PAR)
	Lexeme(line)
		Lexeme(special case when N=0, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(N, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(0, LITERAL)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(==, EQUAL)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(if)
				Lexeme([, OPEN_BRACKET)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(0! = 1, STRING)
				Lexeme(], CLOSE_BRACKET)
	Lexeme(line)
		Lexeme(all other cases, COMMENT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(N, IDENTIFIER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(., DOT)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(0, LITERAL)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(>, GREATER)
	Lexeme(line)
		Lexeme(statement)
			Lexeme(if)
				Lexeme([, OPEN_BRACKET)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(i, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(2, LITERAL)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(=, ASSIGNMENT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(F, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(1, LITERAL)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(=, ASSIGNMENT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(while)
							Lexeme((, OPEN_PAR)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(i, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(., DOT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(N, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(., DOT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(>, GREATER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(^, BREAK)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(F, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(F, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(., DOT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(i, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(., DOT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(*, MULTIPLICATION)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(=, ASSIGNMENT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(i, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(i, IDENTIFIER)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(., DOT)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(1, LITERAL)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(+, ADDITION)
							Lexeme(line)
								Lexeme(statement)
									Lexeme(=, ASSIGNMENT)
							Lexeme(), CLOSE_PAR)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(N, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(!, OUTPUT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(! = , STRING)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(F, IDENTIFIER)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(., DOT)
				Lexeme(line)
					Lexeme(statement)
						Lexeme(!, OUTPUT)
				Lexeme(], CLOSE_BRACKET)
	Lexeme($$, EO_PRG) 
```

# The Mouse Interpreter

The mouse interpreter takes the parse tree from the syntax analyzer and runs it according to the semantics associated with each statement. Comment lines are ignored.  Most statements were already implemented.  You are asked to implement the relational operators, the output, and the assignment statements. This part of your assignment is worth 20 points (each 1 point, assignment is worth 3 points). 

# Coding in Mouse

For the last part of this assignment you are asked to implement a program that computes nCk (number of ways to select r items from a set of n items where the order of selection does not matter). The values of n and k must be provided by the user. This part of the assignment is worth 15 points. This program must be named example9.mouse. 

# Submission

Zip the following, naming the zip as mouse.zip:

* LexicalAnalyzer.scala
* SyntaxAnalyzer.scala
* MouseInterpreter.scala
* example9.mouse

Those are the only files that I am expecting inside your mouse.zip file. 

All source files must be identified with the name(s) of the student(s). 

# Rubric

* 25 Lexical Analyzer
* 50 Syntax Analyzer
* 10 Mouse Interpreter
* 15 nCK in Mouse
* -10 didn't identify the name(s) of the student(s) in all source files 
* -10 didn't submit mouse.zip according to the instructions

Late submission won't be accepted!



