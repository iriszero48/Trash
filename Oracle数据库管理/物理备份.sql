-- 1. 进行Oracle备份策略中最简单的非归档模式下的冷备份，将名为orcl的数据库备份在d:\Orcl\cold\目录下。
-- 2. 进行Oracle备份策略中较为复杂的归档模式下的联机热备份。将名为orcl的数据库备份在d:\Orcl\hot\目录下。
shutdown immediate;
startup;

archive log list;
shutdown immediate;
startup mount;
alter database archivelog;
alter database open;
archive log list;
select  file_name,tablespace_name from dba_data_files;
alter tablespace system begin backup;
host copy D:\ORACLE\ORADATA\ORCL\SYSTEM01.DBF D:\Orcl\hot\;
alter tablespace system end backup;
alter tablespace users begin backup;
host copy D:\ORACLE\ORADATA\ORCL\USERS01.DBF D:\Orcl\hot\;
alter tablespace users end backup;
alter tablespace undotbs1 begin backup;
host copy D:\ORACLE\ORADATA\ORCL\UNDOTBS01.DBF D:\Orcl\hot\;
alter tablespace undotbs1 end backup;
alter tablespace sysaux begin backup;
host copy D:\ORACLE\ORADATA\ORCL\SYSAUX01.DBF D:\Orcl\hot\;
alter tablespace sysaux end backup;
alter system archive log current;
alter database backup controlfile to 'D:\Orcl\hot\control1.ctl';
