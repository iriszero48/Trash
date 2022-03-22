import re
import os
import sys
import oss2
import json
import time
import redis
import queue
import hashlib
import pathlib
import requests

from pathlib import Path
from loguru import logger
from scrapy import Selector
from threading import Thread


class Spider:
    def generate_pages(self):
        response = requests.get(self.url, headers=self.headers)
        html = Selector(response)
        # periodical_list = html.xpath("//select[@name='']/option/@value").getall()
        periodical_list = html.xpath("//div[@class='']//a/@href").getall()
        periodical_list = sorted(list(set(periodical_list)), reverse=True)
        regex = re.compile(r"\\begin{document}.*?\\end{document}")

        for periodical in periodical_list:
            search_params_url = f'{periodical}'
            resp = requests.get(search_params_url, headers=self.headers)
            html = Selector(resp)
            sub_list = html.xpath("//div[@class='']") # /div[@class=''/]
            for sub_html in sub_list:
                title = sub_html.xpath("normalize-space(.//div[@class='']/a)").get()
                url = sub_html.xpath(".//div[@class='']/a/@href").get()
                desc = regex.sub('', sub_html.xpath("normalize-space(.//span[@class=''])").get())
                data = {'title': title, 'desc': desc, 'url': url}
                yield data

    def push_task(self):
        for page in self.generate_pages():
            logger.info(f"成功:{page}")
            self.list_queue.put(page)

    def crawler(self):
        while 1:
            if self.list_queue.qsize():
                data = self.list_queue.get()
                response = requests.get(data['url'], headers=self.headers)
                html = Selector(response)
                data['tag'] = ' '.join([i.strip() for i in html.xpath("//ul[@class='']/li/a/text()").getall()])
                data['id'] = html.xpath("//input[@id='']/@value").get()
                data['download_url'] = f"{data['id']}"
                self.entity_queue.put(data)


if __name__ == '__main__':
    s = Spider()
    s.run()

