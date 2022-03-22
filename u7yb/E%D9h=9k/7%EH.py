import os
import sys
import json
import time
import redis
import shutil
import traceback
import emoji
from bypy import ByPy
from pathlib import Path
from loguru import logger

def get_price(dir_name):
    prefix = dir_name.split('-')[0]
    if prefix[-1] == '':
        return int(prefix[:-1]) * 100, None

    prefix = dir_name.split('-')[1]
    if prefix[-2:] == '':
        return None, int(prefix[:-2])

    logger.error(f'unrecognized price: {dir_name}')
    raise ValueError


def filename_filter(filename):
    arr = []
    pos = filename.find('-')
    if pos >= 0 and filename[:pos] in arr:
        return filename[pos + 1:]
    return filename
