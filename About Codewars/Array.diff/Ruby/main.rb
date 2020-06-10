def array_diff(a, b)
  res=[]
  a.each do |x|
    unless b.include? x
      res.append x
    end
  end
  res
end
