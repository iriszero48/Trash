open System
let count = Convert.ToInt32(Console.ReadLine())
type book = {name:string;price:float}
let mutable max = {name="";price=0.0}
let mutable min = {name="";price=1000000.0}
for i = 1 to count do
    let input = {name=Console.ReadLine();price=Convert.ToDouble(Console.ReadLine())}
    if input.price>max.price then
        max<-input
    if input.price<min.price then 
        min<-input
Console.Write(String.Format("{0:F}",max.price)+", "+max.name+"\n"+String.Format("{0:F}",min.price)+", "+min.name)
