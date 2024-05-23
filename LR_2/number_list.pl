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

%Проверить, является ли число свободным от квадратов.
%call_square_free(+Num)
call_square_free(Num):-square_free(Num,2).
square_free(Num,Num):-!.
square_free(Num, Squarer):-
Square is Squarer*Squarer,
Moded is Num mod Square,
Moded \= 0,
Squarer_temp is Squarer + 1,
square_free(Num,Squarer_temp).

%Реализовать предикат чтения списка с клавиатуры и предикат вывода списка на экран.
%read_list(+N,-List)
read_list(0,[]):-!.
read_list(N,[Head|Tail]) :- read(Head), NewN is N - 1,
    read_list(NewN,Tail).
%write_list(+List)
write_list([]) :- !.
write_list([H|T]) :- write(H), nl, write_list(T).

%Реализовать предикат sum_list_down(+List, ?Summ), sum_list_up(+List, ?Summ), которые проверяют, является ли
%Summ суммой элементов списка или записывает в эту переменную сумму элементов. Выполнить рекурсией вниз и вверх соответственно.
%Рекурсия вниз
%sum_list_down(+List, -Sum)
sum_list_down(List,Sum) :- sum_list_down(List,0,Sum).
sum_list_down([],CurSum,CurSum):-!.
sum_list_down([H|T],CurSum,Sum) :- NewSum is CurSum + H,
    sum_list_down(T,NewSum,Sum).

%read_sum_write(+C)
read_sum_write(C):- read_list(C,List),
sum_list_down(List,Sum),
write(Sum).

%Рекурсия вверх
%sum_list_up(+List,-Sum)
sum_list_up([],0):-!.
sum_list_up([H|T],Sum) :- sum_list_up(T,SumTail), Sum is SumTail + H.

%Построить предикат, который удаляет все элементы, сумма цифр которых равна данной.
%sum_cifr_del(+List,-X,+Number)
sum_cifr_del([],[],Number). 
sum_cifr_del([H|T],X,Number):-cifr_sum_down(H,Sum),
Sum==Number,sum_cifr_del(T,X,Number). 
sum_cifr_del([H|X],[H|Y],Number):-sum_cifr_del(X,Y,Number). 

%Задание 2 Вариант 8

% Найти максимальную цифру числа N
% max_digit(+N, -MaxDigit)
max_digit(N, MaxDigit) :-
    number_codes(N, Digits),
    max_digit_list(Digits, '0', MaxDigitChar),
    atom_number(MaxDigitChar, MaxDigit).
% max_digit_list(+Digits, +CurrentMax, -MaxDigit)
% Рекурсивно находим максимальную цифру в списке цифр Digits
max_digit_list([], MaxDigit, MaxDigit).
max_digit_list([H|T], CurrentMax, MaxDigit) :-
    char_code(H, HCode),
    char_code(CurrentMax, CurrentMaxCode),
    (HCode > CurrentMaxCode -> NewMax = H; NewMax = CurrentMax),
    max_digit_list(T, NewMax, MaxDigit).

% Найти сумму цифр числа N, делящихся на 3
% sum_digits_div_3_list(+Digits, +CurrentSum, -Sum)
% sum_digits_div_3(+N, -Sum)
sum_digits_div_3(N, Sum) :-
    number_codes(N, Digits),
    sum_digits_div_3_list(Digits, 0, Sum).

sum_digits_div_3_list([], Sum, Sum).
% Рекурсивно находим сумму цифр, делящихся на 3 в списке цифр Digits
sum_digits_div_3_list([H|T], CurrentSum, Sum) :-
    atom_number(H, Digit),
    (Digit mod 3 =:= 0 -> NewSum is CurrentSum + Digit; NewSum is CurrentSum),
    sum_digits_div_3_list(T, NewSum, Sum).

% Найти количество делителей числа N
%Рекурсивно переберем все числа от 1 до N и посчитаем количество делителей:
% count_divisors(+N, -Count)
count_divisors(N, Count) :-
    count_divisors(N, 1, 0, Count).
% count_divisors(+N, +Current, +CurrentCount, -Count)
% Рекурсивно находим количество делителей числа N
count_divisors(N, Current, CurrentCount, Count) :-
    Current =< N,
    (N mod Current =:= 0 -> NewCount is CurrentCount + 1; NewCount is CurrentCount),
    Next is Current + 1,
    count_divisors(N, Next, NewCount, Count).
count_divisors(_, Count, Count).

% Задание 5.8
%Найти количество чисел, взаимно простых с заданным.
%Найти делитель числа, являющийся взаимно простым с наибольшим количеством
%цифр данного числа.
% vs_simple(+X,+Y,-Result)
% Проверяет, является ли X простым по отношению к Y.
vs_simple(Number1, Number2, 1) :-
    max(Number1, Number2, MaxNumber),
    min(Number1, Number2, MinNumber),
    vs_simple_rec(MaxNumber, MinNumber).
vs_simple(_, _, 0) :- !.

% vs_simple_rec(+X,+Y)
% Рекурсивно проверяет, если наибольший общий делитель(X, Y) равен 1.
vs_simple_rec(_, 1) :- !, true.
vs_simple_rec(X, Y) :- 0 is X mod Y, !, fail.
vs_simple_rec(X, Y) :-
    NewY is X mod Y,
    NewX is Y,
    vs_simple_rec(NewX, NewY).

% check_simple_count(+N,+Del,-Count)
% Считает количество цифр в N, которые являются простыми по отношению к Del.
check_simple_count(0, _, 0) :- !.
check_simple_count(N, Del, Count) :-
    NewN is N div 10,
    check_simple_count(NewN, Del, PrevCount),
    NewDigit is N mod 10,
    vs_simple(NewDigit, Del, Result),
    Count is PrevCount + Result, !.

% chose_del(+PrevCountMax,+CurrentCount,+CurrentDel,+PrevResultDel,-ResultDel)
% Выбирает делитель, который приводит к наибольшему количеству простых цифр.
chose_del(CurrentCount, CurrentCount, _, PrevResultDel, PrevResultDel) :- !.
chose_del(PrevCountMax, CurrentCount, _, PrevResultDel, ResultDel) :-
    PrevCountMax > CurrentCount, ResultDel is PrevResultDel.
chose_del(_, _, CurrentDel, _, ResultDel) :- ResultDel is CurrentDel, !.

% get_del_down(+N,+CurrentDel,+CurrentMaxCount,+CurrentMaxDel,-ResultDel)
% Рекурсивно пробует делители, уменьшая их до 1, и выбирает лучший делитель.
get_del_down(_, 1, _, ResultDel, ResultDel) :- !.
get_del_down(N, CurrentDel, CurrentMaxCount, CurrentMaxDel, ResultDel) :-
    0 is N mod CurrentDel,
    check_simple_count(N, CurrentDel, NewCount),
    chose_del(CurrentMaxCount, NewCount, CurrentDel, CurrentMaxDel, NewMaxDel),
    max(CurrentMaxCount, NewCount, NewMaxCount),
    NewCurrentDel is CurrentDel - 1,
    get_del_down(N, NewCurrentDel, NewMaxCount, NewMaxDel, ResultDel).
get_del_down(N, CurrentDel, CurrentMaxCount, CurrentMaxDel, ResultDel) :-
    NewCurrentDel is CurrentDel - 1,
    get_del_down(N, NewCurrentDel, CurrentMaxCount, CurrentMaxDel, ResultDel).

% get_del(+N,-Del)
% Del содержит максимальный делитель N, основанный на количестве цифр N, которые являются простыми по отношению к этому делителю.
get_del(N, Del) :- get_del_down(N, N, 0, 0, Del), !.

% read5_8(-InputNumber)
% InputNumber содержит введённое число.
read5_8(InputNumber) :- read(InputNumber), !.

% main5
% Основная процедура
main5 :-
    read5_8(InputNumber),
    get_del(InputNumber, ResultDel),
    write(ResultDel), !.


