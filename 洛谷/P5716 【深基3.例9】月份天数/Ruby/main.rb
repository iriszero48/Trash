y,m=readline.split(" ").map(&:to_i)
puts [1,3,5,7,8,10,12].include?(m) ? 31 : m==2? 28 + (y % 4 == 0 && (y % 100 != 0 || y % 400 == 0) ? 1 : 0) : 30
