import re
from abc import ABC

import scrapy
import lxml
import markdown2
from lxml import etree
from scrapy import signals
from urllib.parse import urljoin


def format_article(article):
    h = etree.HTML(article)
    raw = h.xpath('.//text()')
    md = []
    for i in raw:
        if "" in i:
            md.append(i.replace("", ""))
            continue
        md.append(i)
    html = markdown2.markdown('\n'.join(md))
    return html


class Spider():
    article_count = 0
    redis_key = name + ':start_urls'

    def parse_list(self, response: TextResponse):
        for item in response.xpath("//main[@class='']/li"):
            url = item.xpath(".//a/@href").get()
            self.logger.info(url)
            yield scrapy.Request(urljoin(self.domain, url), callback=self.parse_detail, dont_filter=True)

    def parse_detail(self, response: TextResponse):
        print(f"detail url: {response.url}")
        title = response.xpath("//h3[@class='']/text()").get()
        date_str = response.xpath("//div[@class='']//li[@class='']/text()").get()
        date = clean_date(date_str.split('：')[1])
        aid = response.url
        article = response.xpath("//div[@class='']/div/textarea").get()
        article = format_article(article)
        item = {
            'username': self.username,
            'aid': aid,
            'title': title,
            'date': date,
            'article': article,
            'origin_url': response.url,
            'is_cover': 1
        }
        self.article_count += 1
        print('article_count:', self.article_count)
        yield get_single_article_item(**item)
        print("done")
