create database if not exists test;
use test;
set global local_infile = True;
create table if not exists oceny (
	id int primary key,
	data date,
	id_ucznia char(8) not null,
	id_przedmiotu int not null,
	ocena int
);
load data local infile 'data/oceny.txt' into table oceny fields terminated by '	' ignore 1 lines;
create table if not exists uczniowie (
	id_ucznia char(8) primary key,
	imie varchar(11),
	nazwisko varchar(13),
	klasa varchar(5)
);
load data local infile 'data/uczniowie.txt' into table uczniowie fields terminated by '	' ignore 1 lines;
