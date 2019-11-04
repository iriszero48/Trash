import os
import time
                                
def getCPUtemperature():
    res = os.popen('cat /sys/class/thermal/thermal_zone0/temp').readline()
    return(int(res))
    
def getRAMinfo():
    p = os.popen('free')
    i = 0
    while 1:
        i = i + 1
        line = p.readline()
        if i==2:
            return(line.split()[1:4])

def getCPUuse():
    return(str(os.popen("top -n1 | awk '/Cpu\(s\):/ {print $2}'").readline().strip()))

def getDiskSpace():
    p = os.popen("df -h /")
    i = 0
    while 1:
        i = i +1
        line = p.readline()
        if i==2:
            return(line.split()[1:5])

while 1:
    CPU_temp = round(getCPUtemperature() / 1000,2)
    CPU_usage = getCPUuse()
    RAM_stats = getRAMinfo()
    RAM_total = round(int(RAM_stats[0]) / 1000,2)
    RAM_used = round(int(RAM_stats[1]) / 1000,2)
    RAM_free = round(int(RAM_stats[2]) / 1000,2)
    DISK_stats = getDiskSpace()
    DISK_total = DISK_stats[0]
    DISK_used = DISK_stats[1]
    DISK_perc = DISK_stats[3]
     
    if __name__ == '__main__':
        os.system('clear')
        print('CPU Temperature = '+str(CPU_temp))
        print('CPU Use = '+CPU_usage)
        print('RAM Total = '+str(RAM_total)+' MB')
        print('RAM Used = '+str(RAM_used)+' MB')
        print('RAM Free = '+str(RAM_free)+' MB')
        print('DISK Total Space = '+str(DISK_total)+'B')
        print('DISK Used Space = '+str(DISK_used)+'B')
        print('DISK Used Percentage = '+str(DISK_perc))
    time.sleep(1)
