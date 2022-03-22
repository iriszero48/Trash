import functools

import oss2
import retrying

import lib.uploader


def func_combine(*funcs):
    def impl(*args):
        res = funcs[0](*args)
        for func in funcs[1:]:
            if res is None:
                return None
            res = func(res)
        return res
    return impl


def func_fork(func):
    def ret(args):
        func(args)
        return args
    return ret


class OssFileItem:
    key: str
    file_path: str

    def __init__(self, key: str, file_path: str):
        self.key, self.file_path = key, file_path


class OssUploader(lib.uploader.IUploader):
    bucket: oss2.Bucket

    def __init__(self, bucket: oss2.Bucket):
        self.bucket = bucket

    @retrying.retry(stop_max_attempt_number=7, wait_exponential_multiplier=1000)
    def oss_upload(self, data: OssFileItem) -> OssFileItem:
        if type(data) == OssFileItem:
            self.bucket.put_object_from_file(data.key, data.file_path)
        else:
            raise NotImplementedError(f"oss_upload: '{type(data)}': invalid data found when processing input")
        return data
