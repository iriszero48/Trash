# frozen_string_literal: true

# a = Array.new(128 * 128) { |i| (i + 1.0) / (128 * 128) }
a = (128 * 128).times.map{ rand(0.0..1.0) }
a[128 * 128 - 1] = 1.0
b = Array.new(128 * 128) { |_| 1.0 }
c = Array.new(128 * 128)
sum_value = 0.0

def add(mat, val)
  (0...128 * 128).each {|i| mat[i] += val}
end

def div(mat, val)
  (0...128 * 128).each {|i| mat[i] /= val}
end

def mul(a, b, c)
  (0...128).each { |row|
    (0...128).each { |col|
      v = 0.0
      (0...128).each { |i| v += a[row * 128 + i] * b[i * 128 + col] }
      c[row * 128 + col] = v
    }
  }
end

tp1 = Time.now

(0...1000).each do |i|
  add(b, i)
  mul(a, b, c)

  add(b, i + 1)
  mul(c, b, a)

  div(a, c[127 * 128 + 127])
end


(0...128*128).each {|i| sum_value += a[i]}

tp2 = Time.now

puts "#{(tp2 - tp1) * 1000} #{sum_value}"
