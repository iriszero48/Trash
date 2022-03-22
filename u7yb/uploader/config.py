import os
import platform
import pathlib

import oss2
import yaml
from loguru import logger

with open(os.path.join(os.path.abspath('.'), 'config.yml')) as cfg_f:
    CONFIG = yaml.safe_load(cfg_f.read())

    logger.add(sink=str(pathlib.Path(pathlib.Path.cwd(), 'log') / '{time}.log'),
               rotation='00:00',
               retention='30 days',
               encoding='utf-8',
               enqueue=True,
               )

    logger.debug("Load config file successful.")

OSS_CFG = CONFIG["oss"]
OSS_AKI = OSS_CFG['access_key_id']
OSS_AKS = OSS_CFG['access_key_secret']
OSS_ENDPOINT = OSS_CFG["endpoint"]
OSS_BUCKET_NAME = OSS_CFG["bucket_name"]
OSS_AUTH = oss2.Auth(OSS_AKI, OSS_AKS)
oss_bucket = oss2.Bucket(OSS_AUTH, OSS_ENDPOINT, OSS_BUCKET_NAME)

MS_CFG = CONFIG['mssql']
MS_ADDR = MS_CFG['address']
MS_PORT = MS_CFG['port']
MS_USER = MS_CFG['username']
MS_PW = MS_CFG['password']
MS_DB = MS_CFG['database']

if platform.system() == 'Darwin':
    MS_DRIVER = 'ODBC Driver 13 for SQL Server'
else:
    MS_DRIVER = 'SQL Server'

ms_url = f"DRIVER={{{MS_DRIVER}}};SERVER={MS_ADDR},{MS_PORT};DATABASE={MS_DB};UID={MS_USER};PWD={MS_PW};"
