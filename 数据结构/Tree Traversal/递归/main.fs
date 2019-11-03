type 'T Tree =
    | Empty
    | Node of 'T * 'T Tree * 'T Tree

let rec Preorder f = function
    | Empty -> ()
    | Node(v,l,r) ->
        f v
        Preorder f l
        Preorder f r

let rec Inorder f = function
    | Empty -> ()
    | Node(v,l,r) ->
        Inorder f l
        f v
        Inorder f r

let rec Postorder f = function
    | Empty -> ()
    | Node(v,l,r) ->
        Postorder f l
        Postorder f r
        f v

[<EntryPoint>]
let main _ = 
    let t =
        Node(1,
            Node(2,
                Node(4,
                    Node(7,Empty,Empty),
                    Empty),
                Node(5,Empty,Empty)),
            Node(3,
                Node(6,
                    Node(8,Empty,Empty),
                    Node(9,Empty,Empty)),
                Empty))
    let f = (fun x -> printf "%A " x)
    Preorder f t
    printfn ""
    Inorder f t
    printfn ""
    Postorder f t
    0
