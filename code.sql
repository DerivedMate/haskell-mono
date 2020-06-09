create database if not exists zad98;
use zad98;
set global local_infile = True;

create table if not exists `oceny` (
	Id int,
	Data date,
	Id_ucznia char(8),
	Id_przedmiotu int,
	Ocena int
);
load data local infile 'data/oceny.txt' into table `oceny` fields terminated by '	' ignore 1 lines;


create table if not exists `przedmioty` (
	Id_przedmiotu int,
	Nazwa_przedmiotu varchar(52)
);
load data local infile 'data/przedmioty.txt' into table `przedmioty` fields terminated by '	' ignore 1 lines;


create table if not exists `uczniowie` (
	Id_ucznia char(8),
	Imie varchar(11),
	Nazwisko varchar(13),
	Klasa varchar(5)
);
load data local infile 'data/uczniowie.txt' into table `uczniowie` fields terminated by '	' ignore 1 lines;


