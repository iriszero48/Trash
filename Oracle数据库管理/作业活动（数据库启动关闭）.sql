-- A、启动Oracle数据库，分布启动的三步命令分别是1、 2、 3.
-- B、急需关闭数据库，但需等待已连接的用户提交完事务，用户可不断开会话再关闭，需用什么命令关闭。
-- C、急需关闭数据库，所有已连接的用户中止会话再关闭，需用什么命令关闭。
startup nomount
alter database mount
alter database open

shutdown transactional

shutdown immediate
