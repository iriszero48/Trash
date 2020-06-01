-- 1、查看表空间信息。
-- 2、创建某系统项目的表空间 姓名_tbs(例：张三_tbs),这个表空间初始数据文件初始大小为5MB，设为自动扩展，增长量为每次1MB，最大100M。
-- 3、修改题2所建的数据文件，使其自动扩展增长量为每次10MB，最大300M
-- 4、在2题所建的表空间基础上增加一个数据文件zhangsan_tbs02.dbf,数据文件初始大小为10MB，设为自动扩展，增长量为每次5MB，最大500M.
-- 5、假设表空间中有数据，需要删除表空间及其数据文件。
select * from dba_tablespaces;
create tablespace *_tbs datafile 'tw_tbs_01.dbf' size 5m autoextend on next 1m maxsize 100m;
alter database datafile '*_tbs_01.dbf' autoextend on next 10m maxsize 300m;
alter tablespace *_tbs add datafile 'zhangsan_tbs_02.dbf' size 10m autoextend on next 5m maxsize 500m;
drop tablespace *_tbs including contents and datafiles;
