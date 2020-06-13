def duplicate_count(text)
  count = Hash.new(0)
  text.downcase.scan(/./) { |x| count[x] += 1 }
  count.count { |x| x[1] > 1 }
end
