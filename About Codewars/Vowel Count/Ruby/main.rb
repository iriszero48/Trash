def getCount(inputStr)
  inputStr.chars.count { |x| x =~ /[aeiou]/ }
end
