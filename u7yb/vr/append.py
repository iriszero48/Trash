import pathlib, re, json


def get_white():
    path = ""
    with open(path, 'r') as f:
        return [i.strip() for i in f.readlines() if len(i) > 1]


WhiteList = (lambda: get_white())()
WhiteList.append("Unable to (read|open) key file .+")
WhiteList.append("Error when loading first segment '.+'")
WhiteList.append("Invalid data found when processing input")

WhiteList.append(r"Increasing reorder buffer to \d+")

WhiteList.append(r"decoding for stream \d+ failed")

WhiteList.append(r"error:00000000:lib\(0\):func\(0\):reason\(0\)")
WhiteList.append(r"avforamt_open_input: Input\/output error")

RetryList = []
RetryList.append("error:00000000:lib\(0\):func\(0\):reason\(0\)")


for i in WhiteList:
    print(i)


def chkSubErrOk(sub_error: str):
    for wl in WhiteList:
        if re.search(wl, sub_error) is not None:
            return True
    return False

def chk_retry_err(error: list):
    for sub_error in error:
        for wl in WhiteList:
            if re.search(wl, sub_error) is not None:
                return True
    return False

def chkOk(error: list):
    for err in error:
        if err == r"[json.exception.type_error.304] cannot use at() with null":
            return True
        if not chkSubErrOk(err):
            return False
    return True


def err_filter_check(error):
    for w in WhiteList:
        if not all([re.search(w, i) is None for i in error]):
            return True
    return False


def error_str_to_list(err_str: str):
    error_raw = []
    for i in err_str.split("\n"):
        e = i.strip()
        if len(e) > 0:
            error_raw.append(e)
    error = []
    for i in range(len(error_raw)):
        if error_raw[i].startswith('['):
            error.append(error_raw[i])
        else:
            error[-1] = error[-1] + '\n' + error_raw[i]
    return error


if __name__ == '__main__':
    skip = set()
    data = []
    run_data = set()
    re_try = []
    for err_file in pathlib.Path("").rglob("*.txt"):
        with open(err_file, 'r') as err_fs:
            for err in err_fs.readlines():
                err_obj = json.loads(err)
                    if not chkOk(err_obj['error']):
                        data.append(err_obj)
                    if chk_retry_err(err_obj['error']):
                        re_try.append(err_obj)
    print(len(data))
