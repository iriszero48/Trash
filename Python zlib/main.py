import zlib
import binascii
IDAT = "789C5D91011280400802BF04FFFF5EFCFC0ACF023E77C17C7897667".decode('hex')
result = binascii.hexlify(zlib.decompress(IDAT))
print result
print result.decode('hex')
