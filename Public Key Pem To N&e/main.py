from Crypto.PublicKey import RSA
from tkinter import *
import tkinter.filedialog
print('Public Key Pem To N&e')
root = tkinter.Tk()
root.withdraw()
filename = tkinter.filedialog.askopenfilename()
with open(filename, 'r') as f:
    key = RSA.importKey(f.read())
    N = key.n
    e = key.e
    print('hex:')
    print('N=' + hex(N))
    print('e=' + hex(e))
    print('dec:')
    print('N=' + str(N))
    print('e=' + str(e))
