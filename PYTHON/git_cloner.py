import argparse
import os

import requests
from bs4 import BeautifulSoup
import git


def star_iter(user: str):
    def iter_impl(page_url):
        soup = BeautifulSoup(requests.get(page_url).text, 'html.parser')
        tab = soup.find(id='user-starred-repos')
        for a in tab.select('h3>a'):
            href = a['href'].strip('/')
            yield href
        next_page = tab.find('div', 'paginate-container').select('a')[-1]['href']
        for sub_a in iter_impl(next_page):
            yield sub_a

    for i in iter_impl(f'https://github.com/{user}?tab=stars'):
        yield i


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-r', '--root', default='.')
    parser.add_argument('-u', '--user', required=True)

    args = parser.parse_args()
    root_dir = args.root
    usr = args.user

    for i in star_iter(usr):
        url = f'https://github.com/{i}.git'
        print(f'-> {url}')
        sub_urls = i.split('/')
        assert len(sub_urls) == 2
        auth = sub_urls[0]
        repo = sub_urls[1]

        dest = f'{root_dir}/{auth}/{repo}'
        if not os.path.exists(dest):
            git.Repo.clone_from(url, dest)


if __name__ == '__main__':
    main()
