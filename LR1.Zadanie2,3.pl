man(voeneg).
man(ratibor).
man(boguslav).
man(velerad).
man(duhovlad).
man(svyatoslav).
man(dobrozhir).
man(bogomil).
man(zlatomir).

woman(goluba).
woman(lubomila).
woman(bratislava).
woman(veslava).
woman(zhdana).
woman(bozhedara).
woman(broneslava).
woman(veselina).
woman(zdislava).

parent(voeneg,ratibor).
parent(voeneg,bratislava).
parent(voeneg,velerad).
parent(voeneg,zhdana).

parent(goluba,ratibor).
parent(goluba,bratislava).
parent(goluba,velerad).
parent(goluba,zhdana).

parent(ratibor,svyatoslav).
parent(ratibor,dobrozhir).
parent(lubomila,svyatoslav).
parent(lubomila,dobrozhir).

parent(boguslav,bogomil).
parent(boguslav,bozhedara).
parent(bratislava,bogomil).
parent(bratislava,bozhedara).

parent(velerad,broneslava).
parent(velerad,veselina).
parentparent(veslava,broneslava).
parent(veslava,veselina).

parent(duhovlad,zdislava).
parent(duhovlad,zlatomir).
parent(zhdana,zdislava).
parent(zhdana,zlatomir).

% 2 задание - Построить предикат father(?X, ?Y).
father(X,Y):- man(X), parent(X,Y).
father(X):- father(Y,X), print(Y), nl, fail.

%Построить предикат wife(?X, ?Y).
wife(X,Y) :- woman(X),parent(X,Z), parent(Y,Z), !.
wife(X) :- wife(Y,X), print(Y), nl, fail.

% 3 задание - Построить предикат grand_ma(X,Y), который проверяет,
% является ли X бабушкой Y.

mother(X,Y) :- woman(X), parent(X,Y).
grand_ma(X,Y) :- woman(X), mother(X,Z), parent(Z,Y),!.                            
grand_mas(X):- mother(Y, Z),parent(Z,X), print(Y), nl, fail.

%является ли X и Y дедущкой и внучкой или внучкой и дедушкой.

grand_pa_and_da(X,Y) :- man(X), woman(Y), parent(Z,Y), parent(X,Z).
grand_pa_and_da(X,Y) :- man(Y), woman(X), parent(Y,Z), parent(Z,X).

% является ли X племяницей Y. woman(z) - чтобы 1 раз выдавалось, а не 2
% раза.

sister(X,Y) :- woman(X), woman(Z), parent(Z,X), parent(Z,Y), X\=Y.
niece(X,Y) :- parent(Z,X), sister(Z,Y).
niece(X) :-  niece(Y,X), print(Y), nl, fail.
