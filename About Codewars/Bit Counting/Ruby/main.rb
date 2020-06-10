def count_bits(n)
  n.to_s(2).chars.count {|x| x == '1'}
end
