create database if not exists test;
use test;
set global local_infile = True;


create table if not exists `przedmioty` (
	id_przedmiotu char(13) primary key,
	nazwa_przedmiotu varchar(53)
);
load data local infile 'data/przedmioty.txt' into table `przedmioty` fields terminated by '	' ignore 1 lines;

