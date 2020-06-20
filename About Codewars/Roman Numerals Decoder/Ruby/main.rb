def solution(roman)
  roman.chars
    .map { |x| { 'M'=>1000, 'D'=>500, 'C'=>100, 'L'=>50, 'X'=>10, 'V'=>5, 'I'=>1 }[x] }
    .reduce([0,1001]) { |sp,x| [(x > sp[1] ? sp[0] - 2 * sp[1] + x : sp[0] + x), x] }[0]
end
