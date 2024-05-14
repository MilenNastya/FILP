%Задание 1
%Реализовать предикат max(+X,+Y,+U,-Z), где U максимальное из чисел X, Y и Z.
max(X,Y,Z,X):- X>Y,X>Z,!.
max(X,Y,Z,X):- Y>X,Y>Z,!.
max(_,_,Z,Z).

%Реализовать предикат fact(+N,-X), где X – это факториал первого аргумента с помощью рекурсии вверх, рекурсии вниз.
%Рекурсия вверх
fact(0,1).
fact(N,X):- N>0,
N1 is N-1,
fact(N1,X1),
X is X1*N.

%Рекурсия вниз
fact2(0,X,X).
fact2(N,A,X):- N>0,
    N1 is N-1,
    A1 is A*N,
    fact2(N1,A1,X1),
    X is X1.
call_fact2(N,X):-fact2(N,1,X).

%Найти сумму цифр числа с помощью рекурсии вверх. Найти сумму цифр числа с помощью рекурсии вниз.
%Рекурсия вверх
%sum_num_up(+N,-S).
sum_num_up(0,0):-!.
sum_num_up(N,S):- N1 is N div 10,
sum_num_up(N1,S1),
S is S1+N mod 10.

%Рекурсия вниз
%sum_num_down(+N,-S).
sum_num_down(N,S):-sum_num_down(N,S,0).
sum_num_down(0,S,S):-!.
sum_num_down(N,S,X):-X1 is X + N mod 10,
N1 is N div 10,
sum_num_down(N1,S,X1). 