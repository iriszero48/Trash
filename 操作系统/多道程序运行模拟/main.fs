open System
open System.Threading
open System.Collections.Generic

//线程
type PrElement =
    | Normal of (int * string * int) list
    | Null

let rate = 100 //与真实时间的比例
let mutable pool = [] //线程池
let mutable suspend = 0 //挂起的线程
let mutable IOSystemWork = false //io工作状态
let Processor = new Queue<Async<unit>>() //处理队列
let IOSystem = new Queue<Thread>() //io队列
let Argslize = String.concat " "
let PrintArgs x = "[" + DateTime.Now.ToString() + "] " + Argslize x |> Console.WriteLine

//改变线程池状态的消息队列
let SetPool = 
    new MailboxProcessor<int * PrElement * AsyncReplyChannel<string>>(fun x ->
        let rec loop count = 
            async{
                let! i, v, r = x.Receive()
                let tpool = Array.ofList pool
                tpool.[i] <- v
                pool <- List.ofArray tpool
                r.Reply ""
                return! loop (count + 1)
            }
        loop 0)

//输入线程
let rec Input count =
    match Console.ReadLine().Trim().ToLower() with
    | s when s <> "" -> 
        (s.Split ' '
        |> List.ofArray
        |> List.map (fun i -> 
            let temp = List.ofArray <| i.Split ','
            count, temp.[0], Convert.ToInt32 temp.[1]))::Input (count + 1)
    | _ -> []

//获取线程id
let GetPid p =
    let pid, _, _ = p
    pid

//获取类型
let GetStatus p =
    let _, status, _ = p
    status

//检查线程是否为null
let toPrElement (r : (int * string * int) list) =
    match r.Length with
    | 0 -> Null
    | _ -> Normal r

//处理线程池
let Process _ =
    //获取可处理的io
    let rec GetIO (po : PrElement list) =
        match po with
        | Normal(p)::_ when GetStatus p.Head = "io" ->
            let iosp::iospt = p
            IOSystemWork <- true
            let pid, _, time = iosp
            SetPool.PostAndReply(fun reply -> pid - 1, toPrElement iospt, reply) |> ignore
            suspend <- pid
            IOSystem.Enqueue(
                new Thread(new ThreadStart(fun _ ->
                    ["process"; pid.ToString(); "io"; time.ToString(); "start"] |> PrintArgs
                    Thread.Sleep(time * rate)
                    IOSystemWork <- false
                    suspend <- 0
                    ["process"; pid.ToString(); "io"; time.ToString(); "end"] |> PrintArgs
                )))
        | _::pt -> GetIO pt
        | _ -> ()
    //处理线程
    let sub = function
        | Normal p ->
            match p with
            | sp::spt when GetStatus sp = "p" && GetPid sp <> suspend ->
                let pid, _, time = sp
                Processor.Enqueue(
                    async{
                        ["process"; pid.ToString(); "subprocess"; time.ToString(); "start"] |> PrintArgs
                        Thread.Sleep(time * rate)
                        ["process"; pid.ToString(); "subprocess"; time.ToString(); "end"] |> PrintArgs
                    })
                if not IOSystemWork then
                    GetIO pool
                    if IOSystem.Count > 0 then
                        IOSystem.Dequeue().Start()
                Console.WriteLine()
                SetPool.PostAndReply(fun reply -> pid - 1, toPrElement spt, reply) |> ignore
                Processor.Dequeue() |> Async.RunSynchronously
                match pool 
                    |> List.filter (fun x -> 
                        match x with
                        | Normal _ -> true
                        | _ -> false)
                    |> List.length with
                | 0 ->
                    Console.Write "请按任意键继续. . ."
                    Console.ReadLine() |> ignore
                    exit 0
                | _ -> true
            | _ -> false
        | _ -> false
    //主循环
    let rec loop = function
        | p::_ when sub p -> loop pool
        | _::pt -> loop pt
        | _ -> loop pool
    loop pool
    
//入口
[<STAThread>]
[<EntryPoint>]
let rec main _ =
    try
        let rec input12 _ =
            Console.Write "1.抢占/2.非抢占:"
            match Console.ReadLine().Trim() with
            | "1" -> FScpp.FS() |> ignore
            | "2" -> 
                Console.WriteLine "示例(p=cpu处理, io=使用io (单位:s))\n按优先级从高到底输入\n输入空行=结束输入\np,30 io,40 p,10\np,60 io,30 p,10\np,20 io,40 p,20\n------------------------\n"
                SetPool.Start()
                pool <- Input 1 |> List.map Normal
                Process()
            | _ -> input12()
        input12()
        0
    with
    | _ ->
        Console.WriteLine "Exm???????"
        main [|String.Empty|]
