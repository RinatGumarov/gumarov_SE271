module HuffmanSort = 
    type Tree = 
        | Leaf of char * int
        | Node of int * Tree * Tree

    type HuffmanCompr(symbols: seq<char>, freq: seq<int>) =


        let leafs =
            Seq.zip symbols freq
            |> Seq.toList 
            |> List.map Leaf

        let frequency node = 
            match node with
            | Leaf(_,p) -> p
            | Node(p,_,_) -> p
