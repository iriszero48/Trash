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
        response = requests.get(self.domain, headers=self.headers)
        html = Selector(response)
        pages = int(html.xpath("normalize-space(//span[@class=''])").get().split(' ')[0])
        for i in range(pages):
            page = i + 1
            response = requests.get(f'{self.domain}files/page/{page}.html')
            html = Selector(response)
            item_node = html.xpath("//div[@class='']/div[@class='']/div[@class='']/a")
            for item in item_node:
                title = item.xpath("normalize-space(.)").get()
                url = item.xpath("./@href").get()
                yield {'title': title, 'url': url}

    def push_task(self):
        for page in self.generate_pages():
            logger.info(f"成功:{page}")
            self.list_queue.put(page)

    def crawler(self):
        while 1:
            if self.list_queue.qsize():
                data = self.list_queue.get()
                response = requests.get(f'{self.domain}{data["url"]}', headers=self.headers)
                html = Selector(response)
                desc_node = html.xpath("//div[@class='']")
                desc_list = f'<div>'
                for i in desc_node.xpath(".//text()"):
                    ctx = i.get().strip()
                    if ctx == "Similar:":
                        break
                    desc_list += ctx
                    if ctx == '':
                        desc_list += f'<br />'
                desc_list += '</div>'
                desc = ''.join([
                    '\n' if i == '' else i
                    for i in [i.get().strip() for i in Selector(None, translate_html(desc_list)).xpath(".//text()")]
                ]).strip()
                data['desc'] = desc
                data['tag'] = ' '.join(
                    html.xpath('normalize-space(//meta[@name=""]/@content)').get().split(', '))

                dl_html = html.xpath("//span[@id='']//td")[1].xpath("./div['']/a")[1:]
                dl_items = set()
                for i in dl_html:
                    dl_url = i.xpath("./@href").get()
                    dl_desc = i.xpath("normalize-space(.)").get()
                    if dl_desc in dl_items:
                        continue
                    else:
                        dl_items.add(dl_desc)
                        new_data = data.copy()
                        new_data['download_url'] = dl_url
                        new_desc = Selector(None, translate_html(f'{new_data["title"]}<br/>{dl_desc}')
                                            ).xpath(".//text()").getall()
                        new_data['desc'] = '\n'.join([i for i in new_desc if i != ' ']) + '\n' + data['desc']
                        self.entity_queue.put(new_data)

    def get_download_url(self, page_url):
        headers = self.headers.copy()
        proxies = get_proxies()
        with requests.Session() as req:
            item_url = f"{self.domain}{page_url}"
            req.get(item_url, headers=headers)
            headers['Referer'] = item_url
            url = f""
            response = req.get(url, headers=headers, allow_redirects=False)
            if response.status_code == 302:
                return response.headers['location']
            else:
                logger.error(f"失败 {response.status_code} {page_url}")


if __name__ == '__main__':
    s = Spider()
    s.run()
