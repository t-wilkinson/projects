#!/bin/env python3
import requests
import re
import subprocess
import os
import datetime
import sys
import shutil
from multiprocessing import Pool

URL = "https://dumps.wikimedia.org/enwiki/latest/"

def is_article(name):
    if re.search(r'pages-articles-multistream\d', name):
        if not re.search('.bz2-rss.xml', name):
            return True
    return False


def download_article(name):
    print(f'Downloading {name}')
    path = re.sub(r'^.*href="(.*\.bz2)".*$', r'\1', name)
    article = requests.get(f'{URL}{path}')
    with open(path, 'bw') as f:
        f.write(article.content)
    return path


def worker(byte_r):
    article = str(byte_r)
    if is_article(article):
        return download_article(article)


if __name__ == '__main__':
# check if lbzip2 exists
    cmd_exists = lambda x: shutil.which(x) is not None
    if not cmd_exists('lbzip2'):
        sys.exit('Could not find lbzip2.')

# mkdir curdir for files we will download
    # article_dir = f'{project_dir}/articles/2020-12-24T19:17:22.716815'
    project_dir = os.path.dirname(__file__)
    iso = datetime.datetime.today().isoformat()
    article_dir = f'{project_dir}/articles/{iso}'
    os.mkdir(article_dir)
    os.chdir(article_dir)

    print('Downloading articles.')
    r = requests.get(URL)
    with Pool(processes=8) as pool:
        for path in pool.map(worker, r.content.split(b'\r\n')) if path
            subprocess.Popen(f'lbzip2 -c -d {path}').wait()
            process = subprocess.Popen(f'{project_dir}/json-article {article_dir}/{path}')
            processes.append(process)

    print('Waiting for all processes to finish.')
    for ps in processes:
        ps.wait()

    # should reduce as much storage and computation required before receiving requests to give to frontend
    print('Storing links to one file and compressing.')
    os.chdir(project_dir)
    # incredibly bad performance
    # subprocess.Popen((f'{project_dir}/store-titles.sh'), shell=True).wait()

    print('Finished!')


'''
what is the fastest way to dump a redis database to a compressed file?
    - redis might not be the best option here. Reading the keys is a major constraint.
    - make two links per link?
    - Some concurrency
        - We will have to write all to a single file.
        - Only way this works is using multiple redis databases, writing a file for each one, then combining.
    - hashes?
        - they seem to be more efficient memory wise, but not cpu wise
        - it seems like hashes encode data more efficiently. lookups are O(1)
        - require max hash size of around hash-zipmap-max-entries
        - save each database in its own hash
        - concurrently retrieve database, write to file, concat
        - might save time as we do fewer operations per second
    - multiple redis databases?
    - How do we visualize cubes?
        - total links will be at least 1gb gzipped

We should reduce memory usage as well. Might speed things up a bit. Free up system resources.
Lets try the naive approach and test speed.
If speed is bareable, it is good
use mget

Instead use a set for each title and store where it is connected
'''

