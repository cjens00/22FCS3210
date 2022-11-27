% CS3210 - Principles of Programming Languages - Fall 2022
% Instructor: Thyago Mota
% Description: colors - option-type question example
% Student Name:

:- dynamic fact/2.

member(X, [X | _]).
member(X, [_ | T]) :-member(X, T).

is_option(Question, Options) :-
  (
    fact(Question, Options) ->
      true;
      format('~w?~n', [Question]),
      read(Answer),
      ( member(Answer, Options) -> assert(fact(Question, Options)) )
  ).

begin :- ( is_option('color', [red, green, blue]) -> format('Color entered is primary!~n'); format('Color entered is NOT primary!~n') ).