import pymysql

db_ip = 'mysql'
db_port = '3306'
db_user = 'root'
db_password = 'toor'
db_database = 'c4'
db = pymysql.connect(host=db_ip,
                     user=db_user,
                     password=db_password,
                     database=db_database,
                     charset="utf8mb4")
db_cur = db.cursor()

data_path = 'data.txt'

base_url = ''
