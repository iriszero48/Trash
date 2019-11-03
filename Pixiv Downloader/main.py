from selenium import webdriver
import shutil
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.options import Options
import getpass
import os
import requests

def login(driver):
    username=input('pixiv用户名:')
    password=getpass.getpass()
    driver.get(r'https://accounts.pixiv.net/login?lang=zh&source=pc&view_type=page&ref=wwwtop_accounts_index')
    driver.find_element_by_xpath("//input[@autocomplete='username']").send_keys(username)
    driver.find_element_by_xpath("//input[@autocomplete='current-password']").send_keys(password,Keys.ENTER)

def getWeb(driver,url):
    print(url)
    print('少女祈祷中...')
    driver.get(url)
    return driver.page_source

def getIDBySearch(sour):
    sourSlice=sour[sour.find(r'<section id="js-react-search-mid">'):sour.find(r'</section><input type="hidden" id="js-mount-point-search-result-list"')]
    sourSplit=sourSlice.split(r'<a href="/member_illust.php?mode=medium&amp;illust_id=')
    ids=[]
    for i in sourSplit[1::2]:
        temp=i.split(r'" title="')[0]
        while temp[-1]==r'"' or temp[-1]==' ':
            temp=temp[0:-2]
        ids.append(temp)
        print('[*] id='+temp+' title='+i.split(r'title="')[1].split(r'" class')[0]+' user='+i.split(r'data-user_name="')[1].split(r'"><span class="_2taBWk8">')[0])
    return ids

def getSearchURL(keyword,order,page,mode):
    #return r'https://www.pixiv.net/search.php?word='+keyword+r'&s_mode='+mode+r'&order='+order+r'&p='+page
    if mode=='':
        return r'https://www.pixiv.net/search.php?word='+keyword+'&order='+order+'&p='+page
    else:
        return r'https://www.pixiv.net/search.php?word='+keyword+'&order='+order+'&mode='+mode+'&p='+page

def downloadGIF(id):
    r = requests.get('https://www.pixiv.net/member_illust.php?mode=medium&illust_id='+id, headers={'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:56.0) Gecko/20100101 Firefox/56.0'})
    url=r.text.split(r'pixiv.context.ugokuIllustData  = {"src":"')[1].split(r'","mime_type":')[0].replace('\\','').replace("ugoira600x600", "ugoira1920x1080")
    print('[!] url = '+url)
    r = requests.get(url,headers={'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:56.0) Gecko/20100101 Firefox/56.0','Referer':'https://www.pixiv.net/member_illust.php?mode=medium&illust_id='+id+'_ugoira1920x1080'}) 
    print('[!] requests send')
    with open(url.split('/')[-1], "wb") as code:
        print('[!] open file '+url.split('/')[-1])
        code.write(r.content)
    print('[!] unzip '+url.split('/')[-1])
    shutil.unpack_archive(url.split('/')[-1],extract_dir=url.split('/')[-1].split('.')[0])
    os.remove(url.split('/')[-1])
    print('Done.')

def downloadPicture(url,id):
    r = requests.get(url,headers={'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:56.0) Gecko/20100101 Firefox/56.0','Referer':'https://www.pixiv.net/member_illust.php?mode=medium&illust_id='+id}) 
    print('[!] requests send')
    with open(url.split('/')[-1], "wb") as code:
        print('[!] open file '+url.split('/')[-1])
        code.write(r.content)
    print('Done.')

def downloadPictures(driver,id):
    driver.get('https://www.pixiv.net/member_illust.php?mode=manga&illust_id='+id)
    print('[!] page loaded')
    total=driver.page_source.split('<span class="total">')[1].split('</span>')[0]
    print('[i] total = '+total)
    orgURL=driver.page_source.split('target="_blank" class="')[1].split('<img src="')[1].split('" class="')[0]
    print('[!] orgURL = '+orgURL)
    os.makedirs(id)
    for x in range(0,int(total)):
        url=orgURL.replace('_p0_','_p'+str(x)+'_')
        print('[!] url = '+url)
        r = requests.get(url,headers={'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:56.0) Gecko/20100101 Firefox/56.0','Referer':'https://www.pixiv.net/member_illust.php?mode=manga&illust_id='+id}) 
        print('[!] requests send')
        with open(id+'\\'+url.split('/')[-1], "wb") as code:
            print('[!] open file '+url.split('/')[-1])
            code.write(r.content)
    print('Done.')

def getIDByMember(sour):
    raw=sour.split(r'<ul class="_image-items">')[1].split(r'</ul></div><div class="clear"></div>')[0].split(r'</a><a href="/member_illust.php?mode=medium&amp;illust_id=')
    ids=[]
    for i in raw[1:]:
        tempID=i.split(r'"><h1 class=')[0]
        ids.append(tempID)
        print('[*] id='+tempID+' title='+i.split(r'<h1 class="title" title=')[1].split(r'">')[0])
    return ids

def downloads(driver,ids):
    count=0
    for id in ids:
        count+=1
        driver.get('https://www.pixiv.net/member_illust.php?mode=medium&illust_id='+id)
        tryPRes=driver.page_source.split(r'<div role="presentation" class="')[2].split(r'href="')[1].split(r'" target="_blank"')[0]
        print('[!] try as pucture = '+tryPRes)
        if tryPRes.startswith('http')==True:
            print('[!] type is picture')
            downloadPicture(tryPRes,id)
        else:
            if '<div class=' in tryPRes:
                print('[!] type is active picture')
                downloadGIF(id)
            else:
                print('[!] type is pictures')
                downloadPictures(driver,id)
        print('[!] process '+str(count)+'/'+str(len(ids)))

def member(driver,memberID):
    url='https://www.pixiv.net/member_illust.php?id='+memberID+'&type=all&p='
    pageStart=int(input('page start:'))
    pageEnd=int(input('page end:'))
    idTemp=[]
    for x in range(pageStart,pageEnd+1):
        idTemp.append(getIDByMember(getWeb(driver,url+str(x))))
    idAll=[]
    for i in idTemp:
        for j in i:
            idAll.append(j)
            print(j)
    input("any key to cont")
    downloads(driver,idAll)

def search(driver):
    keyword=input('keyword:')
    pageStart=int(input('page start:'))
    pageEnd=int(input('page end:'))
    inp=int(input('0.按最新排序\n1.按旧排序\n2.按热门程度排序-全站(会员)\n3.按热门程度排序-男(会员)\n4.按热门程度排序-女(会员)\n>'))
    if inp==5:
        order='popular_female_d'
    elif inp==1:
        order='date'
    elif inp==2:
        order='popular_d'
    elif inp==3:
        order='popular_male_d'
    else:
        order='date_d'
    inp=int(input('0.全部\n1.普通\n2.r18\n>'))
    if inp==1:
        mode='safe'
    elif inp==2:
        mode='r18'
    else:
        mode=''
    idTemp=[]
    for x in range(pageStart,pageEnd+1):
        idTemp.append(getIDBySearch(getWeb(driver,getSearchURL(keyword,order,str(x),mode))))
    idAll=[]
    for i in idTemp:
        for j in i:
            idAll.append(j)
            print(j)
    input("any key to cont")
    downloads(driver,idAll)

choose=input('0.不显示浏览器\n1.显示浏览器\n>')
if choose=='1':
    driver=webdriver.Chrome('chromedriver.exe')
else:
    chrome_options = Options()
    chrome_options.add_argument('--headless')
    chrome_options.add_argument('--disable-gpu')
    driver=webdriver.Chrome(executable_path ='chromedriver.exe',chrome_options=chrome_options)
login(driver)
choose=input('0.搜索\n1.id\n2.画师id\n>')
if choose=='2':
    while True:
        member(driver,input('画师id:'))
elif choose=='1':
    while True:
        downloads(driver,[input('id:')])
else:
    while True:
        search(driver)
