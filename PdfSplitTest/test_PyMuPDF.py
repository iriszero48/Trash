import fitz, time

t1 = time.time()

src = fitz.open('test.pdf')
total = len(src)

doc = fitz.open()
doc.insert_pdf(src, from_page=total - 1 - 10, to_page=total - 1)
#input()
doc.save('output.pdf')
doc.close()

src.close()

t2 = time.time()
print(t2 - t1)
