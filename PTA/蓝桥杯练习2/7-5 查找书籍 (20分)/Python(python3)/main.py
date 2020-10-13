list(map(lambda x:print("%.2f, %s" % (x[1],x[0])),(lambda x:[x[-1],x[0]])(sorted([(input(),float(input())) for i in range(int(input()))],key=lambda x:x[1]))))
