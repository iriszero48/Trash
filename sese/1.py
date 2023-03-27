import json

keywords = {}

int_type_keywords = {}

input_path = r''
output_path = r''
key_prefix = ''

with open(output_path, 'w', encoding='utf-8') as out:
    with open(input_path, 'r', encoding='utf-8') as fs:
        cur_row = {}
        cur_key = None
        cur_value = None
        cur_mu_line = False

        def reset_value():
            cur_key = None
            cur_value = None
            cur_mu_line = False

        def reset_row():
            reset_value()
            cur_row = {}

        def save_value():
            if cur_key is not None:
                cur_row[cur_key] = cur_value
                reset_value()

        for line in fs:
            line = line.removesuffix('\r\n').removesuffix('\n')
            if len(line) == 0:
                save_value()
                for k in cur_row:
                    if k in int_type_keywords:
                        cur_row[k] = int(cur_row[k])
                out.write(json.dumps(
                    {f'{key_prefix}_{k}': cur_row[k] for k in cur_row}, ensure_ascii=False) + '\n')

                reset_row()
            elif any([line.startswith(f'{k}:') for k in keywords]):
                line = line.split(':')

                save_value()

                cur_key = line[0]
                cur_value = ':'.join(line[1:]).strip()
                if len(cur_value) == 0:
                    cur_mu_line = True
                    cur_value = []

            elif cur_mu_line and line.startswith('\t'):
                cur_value.append(line.removeprefix('\t').strip())
            else:
                raise Exception(
                    f'unknow token: {json.dumps(line, ensure_ascii=False)}')
