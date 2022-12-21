import glob
import os.path
import pathlib

from loguru import logger
import UnityPy


def fuck_unity(src_path, dst_path):
    ass = UnityPy.load(src_path)
    ass.assets[0].header.version = 17
    with open(dst_path, 'wb') as f:
        f.write(ass.file.save())


def find_sub_string(data, token, start):
    for i in range(len(data) - len(token)):
        if i < start:
            continue
        flag = True
        for j in range(len(token)):
            if data[i + j] != ord(token[j]):
                flag = False
                break
        if flag:
            return i
    return None


@logger.catch
def main():
    src = r'E:\PROJECT\爆裂魔女_解包\2022.6.20\2022.6.20_model\prefab_playermodel'
    dst = r'E:\PROJECT\爆裂魔女_解包\2022.6.20\2022.6.20_model\prefab_playermodel.fix'
    # src = r'C:\Users\iriszero\Documents\MuMu共享文件夹\com.jiansheng.blmn.bilibili\files\patch_res\prefab_uiplayermodel\playerspine'
    # dst = r'C:\Users\iriszero\Desktop\PROJECT\fuck_blmn\export\prefab_uiplayermodel'
    tmp = r'tmp.unity3d'

    os.mkdir(dst)
    for file in glob.glob(f'{src}\*.unity3d'):
        with open(file, 'rb') as fin:
            with open(tmp, 'wb') as fout:
                dt = fin.read()
                token = 'UnityFS'
                fout.write(dt[find_sub_string(dt, token, len(token)):])
        logger.info(file)
        fuck_unity(tmp, rf'{dst}\{pathlib.Path(file).name}')
        os.remove(tmp)


if __name__ == '__main__':
    main()
