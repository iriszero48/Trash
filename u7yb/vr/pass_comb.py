from pathlib import Path

if __name__ == '__main__':
    with open("", "w") as wfs:
        for f in Path("").rglob(""):
            with open(f, "r") as fs:
                for lin in fs.readlines():
                    wfs.writelines(lin)
