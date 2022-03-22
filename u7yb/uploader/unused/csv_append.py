import lib.utility
import glob
import loguru


@loguru.logger.catch
def main():
    data = {}

    for f in glob.glob('...'):
        reader = lib.utility.CsvReader(f)
        for i in reader.items():
            data[i[0]] = i[1]

    writer = lib.utility.CsvWriter('...')
    for k, v in sorted(data.items(), key=lambda x: int(x[0])):
        writer.writerow([k, v])


if __name__ == '__main__':
    main()
