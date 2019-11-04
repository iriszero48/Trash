import os,base64   
strs='U2FsdGVkX190JaZhEvsZ0hLUBXLiV5B8HOM0n6Qenv=='
imgdata=base64.b64decode(strs)  
file=open('t4.key','wb')  
file.write(imgdata)  
file.close()
