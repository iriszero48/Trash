-- 1、创建一个名为res_profile的概要文件，要求每个用户最多可以创建4个并发会话；每个会话持续时间最长为60分钟；如果会话在连续20分钟内空闲，则结束会话；每个会话的私有SQL区为100 KB；每个SQL语句占用CPU时间总量不超过1秒。
-- 2、创建一个名为pwd_profile的概要文件，如果用户连续4次登录失败，则锁定该账户，10天后该账户自动解锁。
create profile res_profile limit sessions_per_user 4 connect_time 60 idle_time 20 private_sga 100k cpu_per_call 100;
create profile pwd_profile limit failed_login_attempts 4 password_lock_time 10;
