import json

import requests

MainHost = 'www.alliedmaker.com'


def SaveList():
    for i in [0, 100, 200]:
        req = requests.get(
            f'http://{MainHost}/api/items?c=5179281&commercecategoryurl=%2FProducts&country=US&currency=USD&fieldset=search&include=facets&language=en&limit=100&n=2&offset={i}&pricelevel=5&sort=displayname%3Aasc&use_pcv=F')
        with open(f'list{i}.json', 'w') as f:
            f.write(json.dumps(req.json()))


def GetUrl():
    for i in [0, 100, 200]:
        with open(f'list{i}.json', 'r') as f:
            raw = json.loads(f.read())
            for item in raw['items']:
                print(item["urlcomponent"])
                yield item["urlcomponent"]


def GetModelUrl(item):
    req = requests.get(
        f'http://{MainHost}/api/items?c=5179281&country=US&currency=USD&fieldset=details&include=facets&language=en&n=2&pricelevel=5&url={item}&use_pcv=F')
    return req.json()["items"][0]["custitem_3d_model"]


def SaveModelUrl():
    with open('model.txt', 'w') as f:
        for i in GetUrl():
            url = GetModelUrl(i)
            f.write(f'{i}\n{url}\n')
            f.write(url)
            print(url)


def GetDownloadPageUrl():
    with open('download.txt', 'w') as f:
        lines = []
        with open('model.txt', 'r') as ff:
            lines = ff.readlines()
        count = len(lines)
        titles = lines[::2]
        urls = lines[1::2]
        for i in range(count // 2):
            title = titles[i].strip()
            url = urls[i].strip()
            f.write(f'{title}\n')
            if not url.startswith("https://a360.co/"):
                f.write(f'{url}\n')
                continue
            req = requests.get(url, allow_redirects=False)
            url = req.headers["Location"]
            f.write(f'{url}\n')
            print(url)


def SendEmail(email):
    lines = []
    with open('download.txt', 'r') as f:
        lines = f.readlines()
    count = len(lines)
    titles = lines[::2]
    urls = lines[1::2]
    for i in range(count // 2):
        title = titles[i].strip()
        url = urls[i].strip()
        req = requests.get(
            f'https://alliedmaker.autodesk360.com/shares/download/{url.split("/")[-1]}/?toFormat=dwg&email={email}')
        print(title, req.status_code)


if __name__ == '__main__':
    SaveList()
    SaveModelUrl()
    GetDownloadPageUrl()
    SendEmail('')
