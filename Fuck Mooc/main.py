import time, re
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.options import Options

def url(id):
    return "http://zswxy.minghuaetc.com/study/unit/" + str(id) + ".mooc"
def getTime():
    return time.strftime("[%Y-%m-%d %H:%M:%S] ")
chrome_options = Options()
chrome_options.add_argument('--autoplay-policy=no-user-gesture-required')
driver=webdriver.Chrome(executable_path ='chromedriver.exe',chrome_options=chrome_options)
driver.get("http://zswxy.minghuaetc.com/home/login.mooc")

start=int(input("start:"))
end=int(input("end:")) + 1
for i in range(start,end):
    try:
        driver.get(url(i))
        if """<i class="i-enter"></i>""" in driver.page_source or """dk0-spocCourseType""" in driver.page_source or "暂无记录！" in driver.page_source:
            print(getTime() + str(i) + " skip.")
        else:
            print(getTime() + str(i) + " fucking class:  " + str(i-start+1) + "...")
            while True:
                sleep=sorted([int(ti.split(':')[0]) for ti in re.findall('''\d{2}\:\d{2}''', driver.page_source)], reverse=True)[0]
                if sleep == 0:
                    time.sleep(1)
                else:
                    break
            sleep += 1
            sleep *= 60
            if "有效分数" not in driver.page_source:
                print('sleep time:' + str(sleep) + " s")
                time.sleep(sleep)
                print(getTime() + str(i-start+1) + " ok.")
            else:
                print(getTime() + str(i) + " skip.")
     
    except Exception as e:
        print(getTime() + str(i) + " " + str(e))
