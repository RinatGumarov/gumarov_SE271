module HuffmanCompression = 
// дерево
// листья дерева хранят в себе символ и сколько раз этот символ встречается(частоту)
// все остальные вершины хранят ссылки на поодеревья и сумму частот поддеревьев
    type Tree = 
        | Leaf of char * int
        | Node of int * Tree * Tree

    type HuffmanCompr(symbols: seq<char>, freq: seq<int>) =

    // список из пар символ*частота(это и есть листья)
        let leafs =
            Seq.zip symbols freq
            |> Seq.toList 
            |> List.map Leaf
   // getFreq
        let frequency node = 
            match node with
            | Leaf(_,p) -> p
            | Node(p,_,_) -> p
