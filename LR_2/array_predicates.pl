%Задание 3 (8 17 34)
%3.1
%Предикат чтения
read_list(List) :-List = [4, 2, 3, 1, 5, 0, 6]. 
%Предикат логики
% find_two_min_indices(+List, -Index1, -Index2)
% Нахождение индексов двух наименьших элементов массива List
find_two_min_indices(List, Index1, Index2) :-
    min_element(List, Min1, Index1), 
    remove_at(List, Index1, RestList),  
    min_element(RestList, Min2, TempIndex2), 
    adjust_index(TempIndex2, Index1, Index2).  

% min_element(+List, -MinElement, -Index)
% Нахождение минимального элемента в списке и его индекса
min_element([H|T], Min, Index) :-
    min_element(T, H, 0, 0, Min, Index).

% min_element(+List, +CurrentMin, +CurrentIndex, +MinIndex, -Min, -Index)
% Рекурсивное нахождение минимального элемента и его индекса
min_element([], Min, _, MinIndex, Min, MinIndex).
min_element([H|T], CurrentMin, CurrentIndex, MinIndex, Min, Index) :-
    NewIndex is CurrentIndex + 1,
    (H < CurrentMin ->
        min_element(T, H, NewIndex, NewIndex, Min, Index)
    ;
        min_element(T, CurrentMin, NewIndex, MinIndex, Min, Index)
    ).

% remove_at(+List, +Index, -ResultList)
% Удаление элемента из списка по индексу
remove_at([_|T], 0, T).
remove_at([H|T], Index, [H|ResultList]) :-
    Index > 0,
    NewIndex is Index - 1,
    remove_at(T, NewIndex, ResultList).

% adjust_index(+TempIndex2, +Index1, -Index2)
% Корректировка индекса второго минимального элемента с учетом удаления первого минимального элемента
adjust_index(TempIndex2, Index1, Index2) :-
    (TempIndex2 >= Index1 ->
        Index2 is TempIndex2 + 1
    ;
        Index2 is TempIndex2
    ).
%Предикат вывода
% print_indices(+Index1, +Index2)
print_indices(Index1, Index2) :-
    format('Первый минимум на позиции: ~d~n', [Index1]),
    format('Второй минимум на позиции: ~d~n', [Index2]).


% main
% Главный предикат для выполнения задачи
main :-
    read_list(List),  % Чтение массива
    find_two_min_indices(List, Index1, Index2),  
    print_indices(Index1, Index2).  

% Задание 3.2

% max_index(+X,+Y,+X_index,+Y_index,-ResultIndex)
max_index(X,Y,_,Y_index,ResultIndex):- max(X,Y,Y), ResultIndex is Y_index,!.
max_index(X,Y,X_index,_,ResultIndex):- max(X,Y,X), ResultIndex is X_index,!.

% max_elem_call(+InputList,-ResultIndex)
max_elem_call([Head|Tail],ResultIndex):- max_elem(Tail,Head,2,1,ResultIndex),!.

% max_elem(+InputList,+CurrentMax,+CurrentIndex,+CurrentResultIndex,-ResultIndex)
max_elem([],_,_,ResultIndex,ResultIndex):-!.
max_elem([Head|Tail],CurrentMax,CurrentIndex,CurrentResultIndex,ResultIndex):- max(Head,CurrentMax,MaxResult), max_index(Head,CurrentMax,CurrentIndex,CurrentResultIndex,NewResultIndex), NewCurrentIndex is CurrentIndex + 1,
max_elem(Tail,MaxResult,NewCurrentIndex,NewResultIndex,ResultIndex).

% putElemOnIndex(+InputList,+IndexToPut,+ElemToPut,-ResultList:List)
putElemOnIndex([_|Tail],IndexToPut,IndexToPut,ElemToPut,[ElemToPut|Tail]):-!.
putElemOnIndex([Head|Tail],CurrentIndex,IndexToPut,ElemToPut,[Head|NewResultList]):- NewIndex is CurrentIndex + 1, putElemOnIndex(Tail,NewIndex,IndexToPut,ElemToPut,NewResultList).


%findViaInd(+InputList,+Index,-Elem)
findViaInd(InputList,Index,Elem):- findViaIndRec(InputList,Index,Elem,1),!.

%findViaIndRec(+InputList,+Index,-Elem,+CurrentInd)
findViaIndRec([Head|_],Index,Head,Index):-!.
findViaIndRec([_|Tail],Index,Elem,CurrentInd):- NewCurrentIndex is CurrentInd + 1, findViaIndRec(Tail,Index,Elem,NewCurrentIndex).

%main_2(+InputList,-ListOtvet)
main_2(InputList,ListOtvet):- min_elem_call(InputList,-1,IndexMin),max_elem_call(InputList,IndexMax), findViaInd(InputList,IndexMin,MinElem),findViaInd(InputList,IndexMax,MaxElem), putElemOnIndex(InputList,1,IndexMin,MaxElem,ResultListMax),putElemOnIndex(ResultListMax,1,IndexMax,MinElem,ListOtvet),!.

%main_call
main_call:-read_list(InputList),main_2(InputList,AnswersList),write('List with switched elements'),nl,write_list(AnswersList).


%Задание 3.3
%read_interval(-Interval)
read_interval([NewStart|NewEnd]):- read(Start),read(End),min(Start,End,NewStart),max(Start,End,NewEnd),!.
%read_3(-InputList,-Interval)
read_3(InputList,Interval):- read_list(InputList),write('This was a list'),nl,read_interval(Interval),!.

%check_values(+InputList,+InputInterval,-Result)
check_values([],_,[]):-!.
check_values([Head|Tail],[Start|End],[Head|PrevResult]):-check_values(Tail,[Start|End],PrevResult),Head>=Start,Head=<End.
check_values([_|Tail],[Start|End],PrevResult):-check_values(Tail,[Start|End],PrevResult).

%main_3(+InputList:List,+Interval:List,-ListOtvet:List)
main_3(InputList,Interval,ListOtvet):-check_values(InputList,Interval,ListOtvet),!.

%main_343
main_343:-read_34(InputList,Interval),main_34(InputList,Interval,ListOtvet),write('Elemnts in your interval'),nl,write_list(ListOtvet),!.


%Задание 4
%Решить логическую задачу по варианту.
%Вариант № 8 Три подруги вышли в белом, зеленом и синем платьях и туфлях.
%Известно, что только у Ани цвета платья и туфлей совпадали. Ни туфли, ни платье Вали не
%были белыми. Наташа была в зеленых туфлях. Определить цвета платья и туфель на каждой
%из подруг.
%in_list(?InputList,?El)
in_list([El|_],El).
in_list([_|Tail],El):-in_list(Tail,El).

%in_list1(+InputList,+El)
in_list1([El|_],El):-!.
in_list1([_|Tail],El):-in_list1(Tail,El).

pr_girlfriends:-Girls = [_,_,_],
    in_list(Girls,[_,green,_]),
    in_list(Girls,[_,blue,_]),
    in_list(Girls,[_,white,_]),
    in_list(Girls,[_,_,green]),
    in_list(Girls,[_,_,blue]),
    in_list(Girls,[_,_,white]),
    in_list(Girls,[vali,_,_]),
    in_list(Girls,[anna,Color1,Color1]),
    in_list(Girls,[natasha,green,_]),
    not(in_list(Girls,[vali,white,_])),
    not(in_list(Girls,[vali,_,white])),

    not(in_list(Girls,[vali,Color2,Color2])),
    not(in_list(Girls,[natasha,Color3,Color3])),
    
    in_list(Girls,[vali,Shoes1,Dress1]),
    in_list(Girls,[anna,Shoes2,Dress2]),
    in_list(Girls,[natasha,Shoes3,Dress3]),
    write("vali:"),write(Shoes1),write(" "), write(Dress1),nl,
    write("anna:"),write(Shoes2),write(" ") ,write(Dress2),nl,
    write("natasha:"),write(Shoes3), write(" "),write(Dress3),nl,!.

%Задание 7.38
%Дан целочисленный массив и отрезок a..b. Необходимо найти количество
%элементов, значение которых принадлежит этому отрезку.
%list_length(+InputList,-ResCount)
list_length([],0):-!.
list_length([_|Tail],ResCount):-list_length(Tail,PrevResCount),ResCount is PrevResCount + 1,!.

%main7_38(+InputList,+Interval,-ResultCount)
main7_38(InputList,Interval,ResultCount):-main_34(InputList,Interval,ListOtvet),list_length(ListOtvet,ResultCount),!.
task7_38:-read_34(InputList,Interval),main7_38(InputList,Interval,ResultCount),write(ResultCount),!.

%Задание 7.46
%Дан целочисленный массив. Необходимо вывести вначале его положительные
%элементы, а затем - отрицательные.
%choose_negative_elements(+InputList,-ResultList)
choose_negative_elements([],[]):-!.
choose_negative_elements([Head|Tail],[Head|NewCurResList]):-choose_negative_elements(Tail,NewCurResList),Head<0,!.
choose_negative_elements([_|Tail],NewCurResList):-choose_negative_elements(Tail,NewCurResList),!.

%choose_positive_elements(+InputList,-ResultList)
choose_positive_elements([],[]):-!.
choose_positive_elements([Head|Tail],[Head|NewCurResList]):-choose_positive_elements(Tail,NewCurResList),Head>0,!.
choose_positive_elements([_|Tail],NewCurResList):-choose_positive_elements(Tail,NewCurResList),!.

%main(+InputList,-ResultList)
main7_46(InputList,ResultList):-choose_negative_elements(InputList,NegativeList),choose_positive_elements(InputList,PositiveList),concat(PositiveList,NegativeList,ResultList),!.

%task7_46
task7_46:-read_list(InputList),main7_46(InputList,ResultList),write_list(ResultList),!.


% Задание 7.50
%Для двух введенных списков L1 и L2 построить новый список, состоящий из
%элементов, встречающихся только в одном из этих списков и не повторяющихся в них.
% del_element(+InputList,+El,-NewList)
del_element([],_,[]):-!.
del_element([El|Tail],El,Tail):-!.
del_element([Head|Tail],El,[Head|PrevResult]):-del_element(Tail,El,PrevResult),!.

%get_unique_elements(+InputList,-ResultList)
get_unique_elements([],[]):-!.
get_unique_elements([Head|Tail],PrevResultList):-get_unique_elements(Tail,PrevResultList), in_list1(PrevResultList,Head),!.
get_unique_elements([Head|Tail],[Head|PrevResultList]):-get_unique_elements(Tail,PrevResultList),!.

%concat_unique(+FirstList,+SecondList,-ResultList)
concat_unique([],SecondList,SecondList):-!.
concat_unique([Head|Tail],SecondList,PrevResultList):-in_list1(SecondList,Head),del_element(SecondList,Head,NewSecondList),concat_unique(Tail,NewSecondList,PrevResultList),!.
concat_unique([Head|Tail],SecondList,[Head|PrevResultList]):-concat_unique(Tail,SecondList,PrevResultList),!.

%read7_50(-FirstList,-SecondList)
read7_50(FirstList,SecondList):-read_list(FirstList),write("Input second list"),nl,read_list(SecondList),!.

%main7_50(+FirstList,+SecondList,-ResultList)
main7_50(FirstList,SecondList,ResultList):-get_unique_elements(FirstList,FirstUnique),get_unique_elements(SecondList,SecondUnique),concat_unique(FirstUnique,SecondUnique,ResultList),!.

%task7_50
task7_50:-read7_50(InputList1,InputList2),main7_50(InputList1,InputList2,ResultList),write_list(ResultList),!.

%Задание 7.56
%Для введенного списка посчитать среднее арифметическое непростых элементов,
%которые больше, чем среднее арифметическое простых.
%get_srt(+ElemSum,+ElemCount,-Result)
get_sr(_,0,0):-!.
get_sr(ElemSum,ElemCount,Result):-Result is ElemSum/ElemCount,!.

%check_if_simple(+N,+CurrentDel)
check_if_simple(1,_):-!,fail.
check_if_simple(N,_):-simple(N),!.
check_if_simple(N,1):-assert(simple(N)),!.
check_if_simple(N,CurrentDel):-0 is N mod CurrentDel,!,fail.
check_if_simple(N,CurrentDel):-NewCurrentDel is CurrentDel - 1,check_if_simple(N,NewCurrentDel).

%get_sum_of_simple(+InputList,-ResultSum,-ResultCount)
get_sum_of_simple([],0,0):-!.
get_sum_of_simple([Head|Tail],CurrentSum,CurrentCount):-get_sum_of_simple(Tail,PrevCurrentSum,PrevCurrentCount),FirstDel is Head - 1,check_if_simple(Head,FirstDel),CurrentSum is PrevCurrentSum + Head,CurrentCount is PrevCurrentCount + 1,!.
get_sum_of_simple([_|Tail],PrevCurrentSum,PrevCurrentCount):-get_sum_of_simple(Tail,PrevCurrentSum,PrevCurrentCount),!.
:-dynamic simple/1.

%get_sum_of_not_simple(+InputList,+Sr,-ResultSum,-ResultCount)
get_sum_of_not_simple([],Sr,0,0):-!.
get_sum_of_not_simple([Head|Tail],Sr,CurrentSum,CurrentCount):-get_sum_of_not_simple(Tail,Sr,PrevCurrentSum,PrevCurrentCount),FirstDel is Head - 1, not(check_if_simple(Head,FirstDel)),Head>Sr,CurrentSum is PrevCurrentSum + Head,CurrentCount is PrevCurrentCount + 1,!.
get_sum_of_not_simple([_|Tail],Sr,PrevCurrentSum,PrevCurrentCount):-get_sum_of_not_simple(Tail,Sr,PrevCurrentSum,PrevCurrentCount),!.

%main7_56(+InputList,-NotSimpleSr)
% NotSimpleSr contains arithmethic average of not simple elements of InputList which are larger than average of simple elements
main7_56(InputList,NotSimpleSr):-retractall(simple(_)),get_sum_of_simple(InputList,ResultSum,ResultCount),get_sr(ResultSum,ResultCount,ResultSr),get_sum_of_not_simple(InputList,ResultSr,NotSimpleSum,NotSimpleCount),get_sr(NotSimpleSum,NotSimpleCount,NotSimpleSr),!.

%task7_56
%main predicate for task 7.56
task7_56:-read_list(InputList),main7_56(InputList,Result),write(Result),!.
