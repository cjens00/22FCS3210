:- dynamic married/2. 

is_true(Q) :- 
    format('~w?~n', Q), 
    read(yes). 

married(john, mary). 
married(mary, john). 

marry(X, Y) :- 
    married(X, _),married(_, X) -> 
        format('~w already married to someone!', X), false;
        married(Y, _),married(_, Y) -> 
            format('~w already married to someone!', Y), false;
            is_true('Are you sure'), 
            is_true('100% sure'), 
            assert(married(X,Y)), 
            assert(married(Y,X)), 
            writeln('I wish you happiness!'). 

divorce(X, Y) :- 
  married(X, Y), 
  married(Y, X), 
  retract(married(X, Y)), 
  retract(married(Y, X)), 
  writeln('Better luck next time...'). 
