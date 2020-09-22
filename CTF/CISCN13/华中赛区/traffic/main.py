nums = [] 
with open('usbdata.txt','r') as keys: 
    posx = 0 
    posy = 0 
    for line in keys:
        x = int(line[2:4],16) 
        y = int(line[4:6],16) 
        if x > 127: 
            x -= 256 
        if y > 127: 
            y -= 256 
        posx += x 
        posy += y 
        btn_flag = int(line[0:2],16)
        if btn_flag == 1: 
            print (posx , posy) 
