let sumTwoSmallestNumbers numbers = 
    [
        numbers 
        |> Array.sort 
        |> Array.head; 
        numbers 
        |> Array.sort 
        |> Array.tail 
        |> Array.head
    ] 
    |> List.sum
