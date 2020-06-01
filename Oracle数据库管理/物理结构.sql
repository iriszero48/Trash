-- 【1】查看Oracle 12c数据库安装后的目录结构，确定控制文件、重做日志文件和数据文件的存储位置。
-- 【2】在SQL*Plus中，查询数据文件的名称和存放路径，以及该数据文件的标识和大小。
-- 【3】在SQL*Plus中，查询当前使用的日志文件组的编号、大小、日志成员数和状态。
-- 【4】在SQL*Plus中，查询控制文件的名称及存储路径。
select file_name from dba_data_files;
select name,bytes,status from v$datafile;
3elect group#,bytes,members,status from v$log;
select name from v$controlfile;
