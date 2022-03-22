from pdfrw import PdfReader, PdfWriter
import time

t1 = time.time()

pdf = PdfReader('test.pdf').pages
total = len(pdf)
outdata = PdfWriter('output.pdf')
for pagenum in range(total - 1 - 10, total - 1):
    outdata.addpage(pdf[pagenum])
#input()
outdata.write()

t2 = time.time()
print(t2 - t1)
