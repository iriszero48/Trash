import os
import sys
import pathlib
from pathlib import Path
from loguru import logger
from bypy import ByPy

import json
import os
import shutil
import redis
import sys
import time
import hashlib
import pathlib
from pathlib import Path
from loguru import logger


def download_files():
    bypy = ByPy(downloader='aria2')
    bypy.downdir(pan_id, str(file_path))
    bypy.remove(pan_id)


if __name__ == '__main__':
    download_files()
    sub_file_path.mkdir(exist_ok=True, parents=True)

    for file in sub_file_path.iterdir():
        fp, file_name = os.path.split(file)
        f, e = os.path.splitext(file_name)
        zip_file_path = Path(fp) / f
        unziped_folder_path = unzip_resource(str(file), del_zip=False)
        upload(unziped_folder_path)

