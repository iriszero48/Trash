def generateHashtag(str)
  res = str.split.map(&:capitalize).join
  res.length >= 140 || res == '' ? false : ('#' + res)
end
