import csv
import os
import subprocess
import re

import fitz
import pandas as pd
from retrying import *

from config import SOURCE_PATH, oss_bucket, nlp_service
from log import logger


def percentage(consumed_bytes, total_bytes):
    if total_bytes:
        rate = int(100 * (float(consumed_bytes) / float(total_bytes)))
        logger.debug('\r{0}% '.format(rate))


@retry(stop_max_attempt_number=7, wait_exponential_multiplier=1000)
def dl_file_impl(oss_key, dl_save_path):
    oss_bucket.get_object_to_file(oss_key, dl_save_path, progress_callback=percentage)


def dl_file(source_id, oss_key):
    if oss_key.startswith('/'):
        oss_key = oss_key[1:]
    _, ext = os.path.splitext(oss_key)
    logger.info(f'{oss_key.split("/")[-1]}{source_id}')
    if not os.path.exists(SOURCE_PATH):
        os.makedirs(SOURCE_PATH)
    dl_save_path = f'{SOURCE_PATH}{source_id}{ext}'

    try:
        dl_file_impl(oss_key, dl_save_path)
    except Exception as ex:
        logger.error(ex)
        os.remove(dl_save_path)
        return None

    if ext != ".pdf":
        conv_path = f'{dl_save_path}.pdf'
        convert2pdf(dl_save_path, conv_path)
        os.remove(dl_save_path)
        dl_save_path = conv_path

    logger.info(f'save as {dl_save_path}')
    return dl_save_path


@retry(stop_max_attempt_number=7, wait_exponential_multiplier=1000)
def get_desc_from_volcengine(text, max_len=0):
    req = {'text': text}
    if max_len != 0:
        req['max_len'] = max_len
    resp = nlp_service.text_summarization(req)
    if 'result' not in resp:
        logger.error(resp)
    return resp['result']


def format_ctx_text(text):
    return ' '.join(text.replace('\n', '').strip().split())


def get_pdf_ctx(file_path):
    with fitz.open(file_path) as doc:
        text, page_text = '', ''

        for index, page in enumerate([page for page in doc.pages()][:20]):
            page_text = format_ctx_text(page.get_text('text'))
            if len(text) + len(page_text) >= 10000:
                break
            text += page_text

        if len(text) == 0:
            return page_text[:10000]

        return text


def format_desc_text(text):
    return re.sub(r'\d{6,}', '', re.sub(r'(\W|[_\n])', '', text))


if __name__ == '__main__':
    data = pd.read_excel(excel_path, sheet_name="Sheet1")

    with open('', 'w', newline='') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()

        oss_keys = set()
        for i in data.index:
            try:
                row = list(data.loc[i].values)
                source_id = row[0]
                username = row[1]
                src_title = row[2]
                src_desc = row[3]
                oss_path = f'{row[4]}{row[5]}'

                if oss_path in oss_keys:
                    logger.warning(f"repeat oss key: '{source_id} => {oss_path}'")
                    continue
                oss_keys.add(oss_path)

                file_path = dl_file(source_id, oss_path)
                pdf_ctx = get_pdf_ctx(file_path)

                volcengine_desc = get_desc_from_volcengine(pdf_ctx)
                volcengine_desc2 = format_desc_text(volcengine_desc)[:100]
                logger.info(result)
                writer.writerow(result)

                os.remove(file_path)
            except Exception as ex:
                logger.error(ex)
