create database c4;
use c4;
create table c4_area
(
	id varchar(12) not null,
	parentid varchar(12) not null,
	name varchar(128) not null,
	customers int default 0 not null,
	constraint c4_area_pk primary key (id)
);
