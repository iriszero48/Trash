import sqlite3
import bs4.element
import requests as req
from bs4 import BeautifulSoup
import datetime
import multiprocessing


def HtmlDump(page):
    with open('text.txt', 'wb') as fs:
        fs.write(page.text.encode(page.encoding))


def ProxyGet(url):
    proxy = f'http://{req.get("").text}'
    return req.get(url, header={}, proxies={'http': proxy, 'https': proxy})
    # return req.get(url)


def GetSubPageData(url):
    page = ProxyGet(url)
    bs = BeautifulSoup(page.text, 'html.parser')
    return bs.find('div', attrs={'class', 'leftMain'}).div.div.div.findAll('a')[-1].contents[0].strip()


def GetData():
    for i in range(22, 26):
        offset = 0
        while True:
            page = ProxyGet(f'')
            bs = BeautifulSoup(page.text, 'html.parser')
            for li in bs.find('h4', attrs={'class': ''}).parent.parent.children:
                if type(li) is not bs4.element.NavigableString:
                    date = li.p.span.get_text()
                    if datetime.datetime.strptime(date, '%Y-%m-%d') < datetime.datetime(2019, 8, 1):
                        yield None
                    tmp = li.a
                    subPageUrl = tmp.get('href')
                    id = int(subPageUrl[subPageUrl.rfind('/') + 1: subPageUrl.rfind('.')])
                    data = {
                        'date': date,
                        'title': tmp.string.strip().replace("\ufeff ", ""),
                        'url': f''
                    }
                    tmp = li.p.next_sibling.next_sibling
                    data['desc'] = tmp.contents[2].string.strip()
                    data['tag'] = [i.get_text()
                                   for i in tmp.next_sibling.next_sibling.children
                                   if type(i) is not bs4.element.NavigableString]
                    data['title'] = GetSubPageData(subPageUrl) + '/' + data['title']
                    yield data
            offset = offset + 10


if __name__ == '__main__':
    db = sqlite3.connect(r'')
    cur = db.cursor()
    for i in GetData():
        print(i)
        cur.execute('insert into test values(?,?,?,?,?)',
                    (i['date'], i['title'], i['url'], i['desc'], '/'.join(i['tag'])))
        db.commit()
