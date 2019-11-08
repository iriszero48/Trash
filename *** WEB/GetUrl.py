import api
from functools import *
import requests
r = requests.get(url=api.GET_QUESTION_STATUS, auth=(api.USER, api.PWD), headers=api.headers, timeout=10, verify=False)
with open('weburl.txt','w') as f:
    f.write(reduce(lambda a,b:a+"\n" +b,[i['attack']['web_ip'] for i in r.json()['AiChallenge']]))
with open('serverurl.txt','w') as f:
    f.write(reduce(lambda a,b:a+"\n" +b,[str(i['defense']) for i in r.json()['AiChallenge']]))
