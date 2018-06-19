:- module(about_lists,
  [my_first/2, my_last/2, my_penultimate/2, my_element_at/3,
   my_number_of/2, my_reverse/2, is_palindrome/1, my_flatten/2,
   my_compress/2, my_pack/2, my_encode/2, my_encode_modified/2,
   my_encode_reverse/2, my_encode_direct/2, my_duplicate/2,
   my_duplicate_for_n/3, my_drop/3, my_split/4, my_slice/4,
   my_rotate/3, remove_at/4, insert_at/4, range/3]).

% Is X the successor of Y?
s(X,Y) :- X #= Y + 1.

% X is the head of the list.
my_first(X,[X|_]).

/* Use "%" at the start of this line for manual implementation.
%% my_last(LastElementOfList, List).
% X is the tail (of the tail (of the tail (...))) of the list.
my_last(X, [X]).
% If there are multiple items, just keep chopping off the head.
my_last(X, [_|T]) :-
    my_last(X, T).
%*/ my_last(X, Xs) :- last(Xs, X).

% Same as above, only stop one step sooner.
my_penultimate(X, [X,_]).
my_penultimate(X, [_|Rest]) :-
    my_penultimate(X, Rest).

/*
% At position 1, X is the head of the list.
my_element_at(X, [X|_],1).
% At position n, X is at position n-1 of the list's tail.
my_element_at(X, [_|Rest],N) :-
    N > 1, % How necessary is this for correctness? Is it just to
           % avoid infinite loops if N<1 is passed?
    N1 is N-1,
    my_element_at(X, Rest, N1).
%*/ my_element_at(X, Xs, I) :- nth1(I, Xs, X).

/*
% Count how many elements are in a list.
my_number_of(0,[]).
my_number_of(N,[_|T]) :-
    my_number_of(M,T),
    s(N,M).
%*/ my_number_of(N,Xs) :- length(Xs, N).

/*
% Reverse a list. Base case: Singleton lists are already reversed.
my_reverse([X], [X]) :- true.
% Recursive case: Appending the head to the reverse of the tail yields
% the reverse.
my_reverse(L1,L2) :-
    [H1 | T1] = L1,
    my_reverse(T1, T2),
    append(T2, [H1], L2).
%*/ my_reverse(Xs, Ys) :- reverse(Xs, Ys).

% Is the list the same when reversed?
is_palindrome(X) :-
    my_reverse(X, X).

/*
%% Flatten a list X such that the output Flat f has the same order of
%% underlying non-list elements but in a single list.
% An empty list goes to itself.
my_flatten([],[]).
% A non-list X maps to the list [X].
my_flatten(X, Flat) :-
    \+ is_list(X), % X is not a list
    Flat = [X].
% Appending a flattened head and a flattened tail yields a flattened list.
my_flatten([H|T], Flat) :-
    my_flatten(H, FlatH),
    my_flatten(T, FlatT),
    append(FlatH, FlatT, Flat).
%*/ my_flatten(Xs, Flat) :- flatten(Xs, Flat).

% Remove all but the first in runs of duplicate elements.
my_compress(Orig, Deduped) :-
    same_sequence(Orig, Deduped),
    compressed(Deduped).
% Do the two lists follow the same sequence?
same_sequence([], []).
same_sequence([X], [X]).
same_sequence(Xs, Ys) :- % duplicate X case
    append(FirstXs, [LastX], Xs),
    append(_, [PenultimateX], FirstXs),
    PenultimateX == LastX,
    same_sequence(FirstXs, Ys).
same_sequence(Xs, Ys) :- % duplicate Y case
    append(FirstYs, [LastY], Ys),
    append(_, [PenultimateY], FirstYs),
    PenultimateY == LastY,
    same_sequence(Xs, FirstYs).
same_sequence(Xs, Ys) :- % same new X and Y case
    append(FirstXs, [LastX], Xs),
    append(FirstYs, [LastY], Ys),
    LastX == LastY,
    same_sequence(FirstXs, FirstYs).

% Are consecutive items always unique?
compressed([]).
compressed([X]).
compressed(Xs) :-
    append(Firsts, [Last], Xs),
    compressed(Firsts),
    append(_, [Penultimate], Firsts),
    Penultimate \== Last.

my_pack(_,_) :- false.

my_encode(_,_) :- false.

my_encode_modified(_,_) :- false.

my_encode_reverse(_,_) :- false.

my_encode_direct(_,_) :- false.

my_duplicate(_,_) :- false.

my_duplicate_for_n(_,_,_) :- false.

my_drop(_,_,_) :- false.

my_slice(_,_,_,_) :- false.

my_split(_,_,_,_) :- false.

my_rotate(_,_,_) :- false.

remove_at(_,_,_,_) :- false.

insert_at(_,_,_,_) :- false.

range(_,_,_) :- false.
