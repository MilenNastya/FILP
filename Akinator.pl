main :-
    nl, write('Тема - фильмы.'), nl,
    retractall(asked(_,_)), fault(Problem),
   !,
    nl,
    write('Это '), write(Problem), write(.), nl.
main :- write('Я не знаю что это за фильм'), nl.

problem('Драма'):- query('Это драматический фильм?').
problem('Комедия'):- query('Это комедия?').
problem('Фантастика'):- query('Это фильм в жанре фантастика?').
problem('Триллер'):- query('Это триллер?').
problem('США'):- query('Фильм снят в США?').
problem('Россия'):- query('Фильм снят в России?').
problem('Турция'):- query('Фильм снят в Турции?').
problem('До2000'):- query('Фильм выпущен до 2000 года?').
problem('После2000'):- query('Фильм выпущен после 2000 года?').
problem('Сериал'):- query('Это сериал?').
problem('Фильм'):- query('Это полнометражный фильм?').
problem('Мультфильм'):- query('Это мультфильм?').
problem('Документальный'):- query('Это документальный фильм?').
problem('Высокий рейтинг'):- query('У фильма/мультфильма высокий рейтинг на Кинопоиске?').
problem('Низкий рейтинг'):- query('У фильма/мультфильма низкий рейтинг на Кинопоиске?').


fault('Лёд'):-
    problem('Драма'),
    problem('Россия'),
    problem('После2000'),
    problem('Фильм');
    problem('Высокий рейтинг') .

fault('Трудные подростки'):-
    problem('Драма'),
    problem('Россия'),
    problem('После2000'),
    problem('Сериал');
    problem('Высокий рейтинг') .

fault('Постучись в мою дверь'):-
    problem('Драма'),
    problem('Турция'),
    problem('После2000'),
    problem('Сериал');
    problem('Высокий рейтинг') .

fault('Ты полюбишь'):-
    problem('Драма'),
    problem('Турця'),
    problem('После2000'),
    problem('Фильм');
    problem('Низкий рейтинг') .

fault('После'):-
    problem('Драма'),
    problem('США'),
    problem('После2000'),
    problem('Фильм');
    problem('Высокий рейтинг') .


fault('Игра пристолов'):-
    problem('Драма'),
    problem('США'),
    problem('После2000'),
    problem('Сериал');
    problem('Высокий рейтинг') .

fault('Мадагаскар'):-
    problem('Комедия'),
    problem('Мультфильм'),
    problem('США'),
    problem('После2000');
    problem('Высокий рейтинг') .

fault('Тарзан'):-
    problem('Комедия'),
    problem('США'),
    problem('До2000'),
    problem('Мультфильм');
    problem('Высокий рейтинг') .

fault('Гарри Поттер и философский камень'):-
    problem('Фантастика'),
    problem('США'),
    problem('После2000'),
    problem('Фильм');
    problem('Высокий рейтинг') .


fault('Звёздные войны'):-
    problem('Фантастика'),
    problem('США'),
    problem('После2000'),
    problem('Сериал');
    problem('Высокий рейтинг') .

fault('Хардкор'):-
    problem('Фантастика'),
    problem('Россия'),
    problem('После2000'),
     problem('Фильм');
    problem('Высокий рейтинг') .

fault('Кибердеревня'):-
    problem('Фантастика'),
    problem('Россия'),
    problem('После2000'),
    problem('Сериал');
    problem('Высокий рейтинг') .



fault('Отряд 2039'):-
    problem('Фантастика'),
    problem('Турция'),
    problem('После2000'),
    problem('Сериал');
    problem('Низкий рейтинг') .

fault('Космический элемент: Эпизод X'):-
    problem('Фантастика'),
    problem('Турция'),
    problem('После2000'),
    problem('Фильм');
    problem('Низкий рейтинг') .

fault('Столкновение'):-
    problem('Триллер'),
    problem('США'),
    problem('После2000'),
    problem('Фильм');
    problem('Высокий рейтинг') .

fault('Хроники Спайдервика'):-
    problem('Триллер'),
    problem('США'),
    problem('После2000'),
    problem('Сериал');
    problem('Низкий рейтинг') .

fault('Поклнник'):-
    problem('Триллер'),
    problem('Россия'),
    problem('До2000'),
    problem('Фильм');
    problem('Высокий рейтинг') .

fault('Пищеблок'):-
    problem('Триллер'),
    problem('Россия'),
    problem('После2000'),
    problem('Сериал');
    problem('Низкий рейтинг') .

fault('Русские хакеры'):-
    problem('Документальный'),
    problem('Россия'),
    problem('После2000'),
    problem('Сериал');
    problem('Высокий рейтинг') .

fault('BEEF:Русский хип-хоп'):-
    problem('Документальный'),
    problem('Россия'),
    problem('После2000'),
    problem('Фильм');
    problem('Низкий рейтинг') .


query(Prompt) :-
    (
        asked(Prompt, Reply) -> true;
        nl, write(Prompt), write(' (y/n)? '), read(X),
        (X = y -> Reply = y; Reply = n),
	assert(asked(Prompt, Reply))
    ),
    Reply = y.
