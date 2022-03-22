import glob
import os
import shutil
import pathlib


if __name__ == '__main__':
    if not pathlib.Path(srv_path).exists():
        os.mkdir(srv_path)
    index = 0
    for i in range(8):
        proc_size = 8
        if i == 7:
            proc_size += 1
        proc_path = f"{srv_path}/proc{i}"
        if not pathlib.Path(proc_path).exists():
            os.mkdir(proc_path)
        for _ in range(proc_size):
            index += 1
            shutil.copyfile(file, f"{proc_path}/{file.split('/')[-1]}")

