-- 1、创建立一个test1用户，密码为test1。默认表空间为system,在该表空间的配额为15MB。使用新创建的用户test1登录数据库，如果不能立即登录，出现错误提示信息，请给出理由。
-- 2、创建立一个test2用户，密码为test2。默认表空间为users,在该表空间的配额为25MB,临时表空间为temp.该用户的口令初始状态为过期，账户初始状态设置为锁定。
-- 3、修改test2用户，将密码改为tiger，默认表空间改为system，账户的状态设置为解锁状态。
-- 4、删除test2用户。
-- 5、查询数据库中所有用户名、默认表空间和临时表空间。
create user test1 identified by test1 default tablespace system quota 15m on system;
conn test1;
conn /as sysdba;
grant create session to test1;
conn test1;
conn /as sysdba;
create user test2 identified by test2 default tablespace users temporary tablespace temp quota 15m on users password expire account lock;
alter user test2 identified by tiger default tablespace system account unlock;
drop user test2;
select username,default_tablespace,temporary_tablespace from dba_users;
