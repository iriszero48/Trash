books={}
for i in range(0,int(input())):
    books[input()]=input()
print("%.2f, %s\n%.2f, %s" % ((float(sorted(books.keys())[-1])),(books[list(sorted(books.keys()))[-1]]),(float(sorted(books.keys())[0])),(books[list(sorted(books.keys()))[0]])))
