import time
from PyPDF2 import PdfFileReader, PdfFileWriter

t1 = time.time()

pdf_file = open('test.pdf','rb')
pdf_reader = PdfFileReader(pdf_file)

total = pdf_reader.getNumPages()

pdf_writer = PdfFileWriter()
for i in range(total-1-10,total-1):
    pdf_writer.addPage(pdf_reader.getPage(i))
split_motive = open('output.pdf', 'wb')
#input()
pdf_writer.write(split_motive)
split_motive.close()

pdf_file.close()

t2 = time.time()
print(t2 - t1)
