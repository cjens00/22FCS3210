% CS3210 - Principles of Programming Languages - Fall 2022
% Instructor: Thyago Mota
% Description: animal - a simple ES based on animals
% Student Name:

:- dynamic fact/1.

is_true(Question) :-
  (
    fact(Question) ->
      true;
      format('~w?~n', [Question]),
      Answer = read(yes),
      ( Answer -> assert(fact(Question)) )
  ).

animal(dog)  :- is_true('has fur'),      is_true('says woof').
animal(cat)  :- is_true('has fur'),      is_true('says meow').
animal(duck) :- is_true('has feathers'), is_true('says quack').

begin :- ( animal(A) -> format('I think your animal is a ~w.~n', [A]); false ).
