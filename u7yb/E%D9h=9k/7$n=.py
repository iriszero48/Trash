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
        menu = []
        for item in menu:
            response = requests.get(f'{self.domain}{item}{0}', headers=self.headers)
            html = Selector(response)
            last_url = html.xpath("//div[@class='']/ul/li[@class='']//@href").get()
            last = int(last_url.split('page=')[-1])
            for i in range(last + 1):
                response = requests.get(f'{self.domain}{item}{i}', headers=self.headers)
                html = Selector(response)
                for td in html.xpath("//div[@class='']//a"):
                    url = td.xpath('.//@href').get()
                    title = td.xpath('normalize-space(.)').get()
                    yield {'url': url, 'title': title, 'cat': item}

    def push_task(self):
        for page in self.generate_pages():
            logger.info(f"成功:{page}")
            self.list_queue.put(page)

    def crawler(self):
        while 1:
            if self.list_queue.qsize():
                data = self.list_queue.get()
                base_url = self.domain[:-1]
                url = f'{base_url}{data["url"]}'
                response = requests.get(url, headers=self.headers)
                if response.status_code != 200:
                    logger.warning(f'失败 {response.status_code} {url}')
                    continue
                html = Selector(response)
                desc_node = html.xpath("//div[@class='']")
                desc_node = translate_html(desc_node.get())
                desc_node = Selector(None, desc_node)
                desc_main = desc_node.xpath(
                    "normalize-space(.//div[@class=''])").get()
                files_list_node = desc_node.xpath(
                    ".//section[@class='']")
                files_list_title = files_list_node.xpath("normalize-space(.//h2)").get()
                files_list_table_title = ' '.join([
                    th.xpath("normalize-space(.)").get()
                    for th in files_list_node.xpath(".//div/div/table/tr/th")
                ])
                files_list_items = '\n'.join([
                    ' '.join([td.xpath("normalize-space(.)").get() for td in tr.xpath(".//td")])
                    for tr in files_list_node.xpath(
                        ".//div/div/table/tr")
                ])
                data['desc'] = f'{desc_main}\n{files_list_title}\n{files_list_table_title}{files_list_items}'
                data['tag'] = ' '.join([
                    i.xpath('normalize-space(.)').get()
                    for i in desc_node.xpath(
                        ".//section[@class='']/ul/li")
                ])
                data['download_url'] = html.xpath(
                    "//div[@class='']//table//a/@href").get()
                data['title'] = f"{data['cat']}-{translate_str(data['title'])}"
                self.entity_queue.put(data)


if __name__ == '__main__':
    s = Spider()
    s.run()
