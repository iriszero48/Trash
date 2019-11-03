open Fuck
open System

//文件系统
type Tree = 
    | Node of list<string * Tree>
    | Leaf of File

//创建文件
let touch disk cpath filename =
    let rec loop d p =
        match d,p with
        | Node((dht, Node(dhn))::_),(ph::[]) when ph = dht -> Node[dht,Node([(filename,Leaf(new File(filename)))]@dhn)]
        | Node((dht,Node(dhn))::_),(ph::pt) when ph = dht -> match loop (Node(dhn)) pt with | Node(x) -> Node((ph,Node(dhn))::x)
        | Node(h::t),_ -> match loop (Node(t)) p with | Node(x) -> Node(h::x)
    loop disk cpath

//删除文件
let rm disk cpath filename =
    let rec loop d p =
        match d,p with
        | Node((dht, Node(dhn))::_),(ph::[]) when ph = dht -> Node[dht,Node(dhn |> List.filter (fun x -> fst x <> filename))]
        | Node((dht,Node(dhn))::t),(ph::pt) when p.Head = dht -> match loop (Node(t)) pt with | Node(x) -> Node((ph,Node(dhn))::x)
        | Node(h::t),_ -> match loop (Node(t)) p with | Node(x) -> Node(h::x)
    loop disk cpath

//打开文件
let fopen disk cpath filename user =
    let rec loop d p =
        match d,p with
        | Node((dht, Node(dhn))::_),(ph::[]) when ph = dht -> match (snd (dhn |> List.filter (fun x -> fst x = filename)).Head) with | Leaf(f) -> f.Open(user)
        | Node((dht,Node(dhn))::_),(_::pt) when p.Head = dht -> loop (Node(dhn)) pt
        | Node(_::t),_ -> loop (Node(t)) p
    loop disk cpath

//关闭文件
let fclose disk cpath filename user =
    let rec loop d p =
        match d,p with
        | Node((dht, Node(dhn))::_),(ph::[]) when ph = dht -> match (snd (dhn |> List.filter (fun x -> fst x = filename)).Head) with | Leaf(f) -> f.Close(user)
        | Node((dht,Node(dhn))::_),(_::pt) when p.Head = dht -> loop (Node(dhn)) pt
        | Node(_::t),_ -> loop (Node(t)) p
    loop disk cpath

//读取文件
let read disk cpath filename user offset =
    let rec loop d p =
        match d,p with
        | Node((dht, Node(dhn))::_),(ph::[]) when ph = dht -> match (snd (dhn |> List.filter (fun x -> fst x = filename)).Head) with | Leaf(f) -> f.Read(user, offset)
        | Node((dht,Node(dhn))::_),(_::pt) when p.Head = dht -> loop (Node(dhn)) pt
        | Node(_::t),_ -> loop (Node(t)) p
    loop disk cpath

//写入文件
let write disk cpath filename user data =
    let rec loop d p =
        match d,p with
        | Node((dht, Node(dhn))::_),(ph::[]) when ph = dht -> match (snd (dhn |> List.filter (fun x -> fst x = filename)).Head) with | Leaf(f) -> f.Write(user, data)
        | Node((dht,Node(dhn))::_),(_::pt) when p.Head = dht -> loop (Node(dhn)) pt
        | Node(_::t),_ -> loop (Node(t)) p
    loop disk cpath

//列出文件和目录
let ls disk cpath =
    let rec loop d p =
        match d,p with
        | Node((dht, Node(dhn))::_),(ph::[]) when ph = dht -> dhn |> List.iter (fun x -> fst x |> Console.WriteLine)
        | Node((dht,Node(dhn))::_),(ph::pt) when ph = dht -> loop (Node(dhn)) pt
        | Node(_::t),_ -> loop (Node(t)) p
    loop disk cpath

//设置用户文件状态
let rec setFilename user filename = function
    | (u, _)::t when u = user -> (u, filename)::t
    | h::t -> h::setFilename user filename t

//读取用户文件状态
let rec getFilename user = function
    | (u,f)::_ when u = user -> f
    | _::t -> getFilename user t

//主循环
let rec system users user disk cpath =
    Console.Write "->"
    match Console.ReadLine().Split(' ') |> List.ofArray with
    | "touch"::f::[] ->
        system users user (touch disk cpath f) cpath
    | "rm"::f::[] -> system users user (rm disk cpath f) cpath
    | "open"::f::[] when getFilename user users = "" && fopen disk cpath f user -> system (setFilename user f users) user disk cpath
    | "close"::f::[] when getFilename user users = f ->
        fclose disk cpath f user
        system (setFilename user "" users) user disk cpath
    | "read"::f::o::[] when getFilename user users = f ->
        read disk cpath f user (Int32.Parse o)
        system users user disk cpath
    | "write"::f::d::[] when getFilename user users = f ->
        write disk cpath f user d
        system users user disk cpath
    | "ls"::[] ->
        ls disk cpath
        system users user disk cpath
    | "cd"::".."::[] -> system users user disk (cpath |> List.take (cpath.Length - 1))
    | "cd"::d::[] -> system users user disk (cpath@[d])
    | "su"::u::[] -> system users u disk cpath
    | "useradd"::u::[] -> system ((u, "")::users) user disk cpath
    | "whoami"::[] -> 
        Console.WriteLine user
        system users user disk cpath
    | "pwd"::[] -> 
        cpath |> List.reduce (fun a b -> a + "/" + b) |> Console.WriteLine
        system users user disk cpath
    | [] -> 
        Console.WriteLine()
        system users user disk cpath
    | e -> 
        Console.WriteLine("Command '" + (List.reduce (fun a b -> a + " " + b) e) + "' not found")
        system users user disk cpath

//入口
[<STAThread>]
[<EntryPoint>]
let main _ = 
    let disk =
        Node["root", Node[
            "someFile.txt", Leaf(new File("someFile.txt", "some content"));
            "aFile.txt", Leaf(new File("aFile.txt", "some content"));
            "someDir", Node[ 
                "anotherFile.txt", Leaf(new File("anotherFile.txt", "more content"));
                "script.sh", Leaf(new File("script.sh", "script content"))];
            "logfile.log", Leaf(new File("logfile.log", "log content"));
            "otherDir", Node[ 
                "otheranotherFile.txt", Leaf(new File("otheranotherFile.txt", "more content"));
                "otherscript.sh", Leaf(new File("otherscript.sh", "script content"))];
            "otherlogfile.log", Leaf(new File("otherlogfile.log", "log content"))]]
    system ["root", ""] "root" disk ["root"]
    0
