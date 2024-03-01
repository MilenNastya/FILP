main :-
    retractall(asked(_,_)),
    fault(Problem),
    !,
    nl,
    write('The problem is '), write(Problem), write(.), nl.
main :-
    nl,
    write('The problem cannot be recognized.'), nl.

problem("����� ���������"):-query('��������� ���������� ����� 3.5 �����?').
problem("���������"):-query('��������� ���������� ����� 4.0 �����?').
problem("����� ��������� � �������"):-query('��������� ���������� ����� 4,7-5,4 �����?').
problem("����� ������� � ���������"):-query('��������� ���������� ����� 5,5 -5.7 �����?').
problem("C������"):-query('��������� ���������� ����� 5,8-6.1 �����').
problem("�������"):-query('��������� ���������� ����� 6.5 -6.7?').

problem("��������� ��� 3 ������"):-query('��������� ���������� Samsung APL0298C05?').
problem("��������� ��� 4 ������"):-query('��������� ���������� Apple A4?').
problem("��������� ��� 5 ������"):-query('��������� ���������� pple A6?').
problem("��������� ��� 5s ������"):-query('��������� ���������� Apple A7?').
problem("��������� ��� 6 ������"):-query('��������� ���������� Apple A8?').
problem("��������� ��� 6s ������"):-query('��������� ���������� Apple A9?').
problem("��������� ��� 7/7plus ������"):-query('��������� ���������� Apple A10 Fusion?').
problem("��������� ��� 8/8 plus/X ������"):-query('��������� ���������� Apple A11 Bionic?').
problem("��������� ��� XR ������"):-query('��������� ���������� Apple A12 Bionic?').
problem("��������� ��� 11 ������"):-query('��������� ���������� Apple A13 Bionic?').
problem("��������� ��� 12 ������"):-query('��������� ���������� Apple A14 Bionic?').
problem("��������� ��� 13 ������"):-query('��������� ���������� Apple A15 Bionic?').
problem("��������� ��� 14 ������"):-query('��������� ���������� Apple A16 Bionic?').
problem("��������� ��� 15 ������"):-query('��������� ���������� Apple A17 Bionic?').

problem("��������� 3"):-query('� ������ ���������� ������ ��������� ��������� ����� �������� �������� ��������� 3G?').
problem("��������� 4"):-query('� ������ ���������� ������ ��������� ����������� ������?').
problem("��������� 5"):-query('� ������ ���������� ������ �������� 4-� �������� �������?').
problem("��������� 5s"):-query('� ������ ���������� ������ ��������� ������ �� 64 ��?').
problem("��������� 6"):-query('� ������ ���������� ������ ��������� ������ �� 128 ��?').
problem("��������� 7"):-query('� ������ ���������� ������ ��������� ����������� ������ �� �������������� ������?').
problem("��������� 8"):-query('� ������ ���������� ������ �������� True Tone Display?').
problem("��������� X"):-query('� ������ ���������� ������ ��������� "�����"?').
problem("��������� XR"):-query('� ������ ���������� ����� �� ����� ��������������, ��� � X, �� �� �������?').
problem("��������� 11"):-query('� ������ ���������� ������ ��������� ����������� ������ ������?').
problem("��������� 12"):-query('� ������ ���������� ������ ��������� ����������� ������� �����/���������� � 5-������� ���������� �����?').
problem("��������� 13"):-query('� ������ ���������� ������ ��������� ������� ���������� ������� -120 ��? ').
problem("��������� 14"):-query('� ������ ���������� ����� �� ��������������, ��� � 13?').
problem("��������� 14 PRO MAX"):-query('� ������ ���������� ������ �������� Dynamic Island? ').
problem("��������� 15"):-query('� ������ ���������� ��������� ������ ��� �������?').
problem("��������� 15 PRO MAX"):-query('���� ���������� ��������� � ��������� �������?').

fault('Iphone 3') :-
    problem("����� ���������"),
    problem("��������� ��� 3 ������"),
    problem("��������� 3").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.


fault('Iphone 4') :-
    problem("����� ���������"),
    problem("��������� ��� 4 ������"),
    problem("��������� 4").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.


fault('Iphone 5') :-
    problem("���������"),
    problem("��������� ��� 5 ������"),
    problem("��������� 5").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.


fault('Iphone 5s') :-
    problem("���������"),
    problem("��������� ��� 5s ������"),
    problem("��������� 5s").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.


fault('Iphone 6') :-
    problem("����� ��������� � �������"),
    problem("��������� ��� 6 ������"),
    problem("��������� 6s").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.


fault('Iphone 7') :-
    problem("����� ��������� � �������"),
    problem("��������� ��� 7/7plus ������"),
    problem("��������� 7").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.

fault('Iphone 7 plus') :-
    problem("����� ������� � ���������"),
    problem("��������� ��� 7/7plus ������"),
    problem("��������� 7").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.


fault('Iphone 8') :-
    problem("����� ��������� � �������"),
    problem("��������� ��� 8/8plus/X ������"),
    problem("��������� 8").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.


fault('Iphone 8 plus') :-
    problem("����� ������� � ���������"),
    problem("��������� ��� 8/8plus/X ������"),
    problem("��������� 8").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.


fault('Iphone X') :-
    problem("�������"),
    problem("��������� ��� 8/8plus/10 ������"),
    problem("��������� X").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.


fault('Iphone XR') :-
    problem("�������"),
    problem("��������� ��� XR ������"),
    problem("��������� XR").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.


fault('Iphone 11') :-
    problem("�������"),
    problem("��������� ��� 11 ������"),
    problem("��������� 11").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.


fault('Iphone 12') :-
    problem("�������"),
    problem("��������� ��� 12 ������"),
    problem("��������� 12").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.


fault('Iphone 13') :-
    problem("�������"),
    problem("��������� ��� 13 ������"),
    problem("��������� 13").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.


fault('Iphone 13 PRO MAX') :-
    problem("�������"),
    problem("��������� ��� 13 ������"),
    problem("��������� 13").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.


fault('Iphone 14') :-
    problem("�������"),
    problem("��������� ��� 14 ������"),
    problem("��������� 14").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.


fault('Iphone 14 PRO MAX') :-
    problem("�������"),
    problem("��������� ��� 14 ������"),
    problem("��������� 14 PRO MAX ").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.

fault('Iphone 15') :-
    problem("�������"),
    problem("��������� ��� 15 ������"),
    problem("��������� 15").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.


fault('Iphone 15 PRO MAX') :-
    problem("�������"),
    problem("��������� ��� 15 ������"),
    problem("��������� 15 PRO MAX").
    bagof(X,asked(X,y),L),
    length(L,A),
    A=3,!.



query(Prompt) :-
    (   asked(Prompt, Reply) -> true
    ;   nl, write(Prompt), write(' (y/n)? '),
        read(X),(X = y -> Reply = y ; Reply = n),
	assert(asked(Prompt, Reply))
    ),
    Reply = y.



