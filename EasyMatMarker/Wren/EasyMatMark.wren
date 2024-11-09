import "random" for Random

var a = []
var b = []
var c = []

var sum_value = 0.0

// for (i in 0..128*128-1) a.add((i + 1) / (128 * 128))
var random = Random.new()
for (i in 0..128*128-1) a.add(random.float())
a[128 * 128 - 1] = 1.0
for (i in 0..128*128-1) b.add(1)
for (i in 0..128*128-1) c.add(0)

var tp1 = System.clock

for (i in 0..1-1) {
    for (j in 0..128*128-1) b[j] = b[j] + i
    for (row in 0..128-1) {
        for (col in 0..128-1) {
            var v = 0
            for (j in 0..128-1) {
                v = v + a[row * 128 + j] * b[j * 128 + col]
            }
            c[row * 128 + col] = v
        }
    }

    for (j in 0..128*128-1) b[j] = b[j] + i + 1
    for (row in 0..128-1) {
        for (col in 0..128-1) {
            var v = 0
            for (j in 0..128-1) {
                v = v + c[row * 128 + j] * b[j * 128 + col]
            }
            a[row * 128 + col] = v
        }
    }

    var div_val = c[127 * 128 + 127]
    for (j in 0..128*128-1) a[j] = a[j] / div_val
}

for (x in a) sum_value = sum_value + x

var tp2 = System.clock
var dur = (tp2 - tp1) * 1000.0
System.print("%(dur) %(sum_value)")