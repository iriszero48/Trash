import sys, base64, os, traceback

def Interactive(func):
    while True:
        print('<-', end = '')
        print('->' + func(input()))

def Rename(func, lst):
    for f in lst:
        try:
            nf = func(os.path.basename(f))
            print(f + '=>' + nf)
            os.rename(f, os.path.join(os.path.dirname(f), nf))
        except Exception as e:
            traceback.print_exc()

Bind1st = lambda func, arg1: lambda arg2: func(arg1, arg2)

Compose = lambda f, g: lambda x: f(g(x))

Get = lambda func, path: [f for f in map(Bind1st(os.path.join, path), os.listdir(path)) if func(f)]

GetFiles = Bind1st(Get, os.path.isfile)
GetDirectories = Bind1st(Get, os.path.isdir)
GetFilesAndDirectories = Bind1st(Get, lambda x: True)

Encode = lambda data: base64.b64encode(data.encode()).decode().replace('+','.').replace('/','(').replace('=',')')
Decode = lambda data: base64.b64decode(data.replace('.','+').replace('(','/').replace(')','=')).decode()

EncodeFiles = Compose(Bind1st(Rename, Encode), GetFiles)
DecodeFiles = Compose(Bind1st(Rename, Decode), GetFiles)

EncodeDirectories = Compose(Bind1st(Rename, Encode), GetDirectories)
DecodeDirectories = Compose(Bind1st(Rename, Decode), GetDirectories)

EncodeFilesAndDirectories = Compose(Bind1st(Rename, Encode), GetFilesAndDirectories)
DecodeFilesAndDirectories = Compose(Bind1st(Rename, Decode), GetFilesAndDirectories)

def Recursive(func, path):
    for d in GetDirectories(path):
        Recursive(func, d)
    func(path)

EncodeFilesRec = Bind1st(Recursive, EncodeFiles)
DecodeFilesRec = Bind1st(Recursive, DecodeFiles)

EncodeDirectoriesRec = Bind1st(Recursive, EncodeDirectories)
DecodeDirectoriesRec = Bind1st(Recursive, DecodeDirectories)

EncodeFilesAndDirectoriesRecRec = Bind1st(Recursive, EncodeFilesAndDirectories)
DecodeFilesAndDirectoriesRecRec = Bind1st(Recursive, DecodeFilesAndDirectories)

if __name__ == "__main__":
    if len(sys.argv) == 2:
        Interactive(
            {
                'es': Encode,
                'ds': Decode,
            }.get(sys.argv[1]))
    elif len(sys.argv) == 3:
        {
            'ef': EncodeFiles,
            'df': DecodeFiles,
            'ed': EncodeDirectories,
            'dd': DecodeDirectories,
            'efd': EncodeFilesAndDirectories,
            'dfd': DecodeFilesAndDirectories,
            'efr': EncodeFilesRec,
            'dfr': DecodeFilesRec,
            'edr': EncodeDirectoriesRec,
            'ddr': DecodeDirectoriesRec,
            'efdr': EncodeFilesAndDirectoriesRecRec,
            'dfdr': DecodeFilesAndDirectoriesRecRec,
        }.get(sys.argv[1])(sys.argv[2])
    else:
        sys.exit(1)
