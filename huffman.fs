module HuffmanCompression = 
    open System.IO
// дерево
// листья дерева хранят в себе символ и сколько раз этот символ встречается(частоту)
// все остальные вершины хранят ссылки на поодеревья и сумму частот поддеревьев
    
    type Tree = 
        | Leaf of char * int
        | Node of int * Tree * Tree

    let input = File.ReadAllText("input.txt")

    let c = input.ToCharArray()

    System.Console.WriteLine()

    let listOfChars = seq[for i in input -> (i, 1)]
    //let listOfChars2 list = seq[for i in list -> if 

    System.Console.WriteLine(listOfChars)


    type HuffmanCompr(symbols: seq<char>, freq: seq<int>) =

    
    // список из пар символ*частота(это и есть листья)
        let leafs =
            Seq.zip symbols freq
            |> Seq.toList 
            |> List.map Leaf
   // getFreq
        let freq node = 
            match node with
            | Leaf(_,p) -> p
            | Node(p,_,_) -> p


            //создание дерева по списку из листьев
            //нижний уровень - листья(символ,частота)
            //сортируем элемент по частоте, чтобы выбрать два с наименьшей
            //создаем новую вершину по принципу - вешины с наименьшей частотой - сыновья, частота = сумма частот сыновей


        let rec buildTree roots =
            match roots |> List.sortBy freq with
            | [] -> failwith "Error! empty list"
            | [node] -> node
            | minmin::min::rest -> 
                let newNode = Node(freq minmin + freq min, minmin, min)
                buildTree (newNode::rest)
        
        
        let tree = buildTree leafs

        // шифрование символов следующим образом
       //           [корень]
       //       (1) |     | (0)
       //     [вершина]   [вершина]
        let huffmanCodeTable =
           let rec huffmanCodes tree =
               match tree with
               | Leaf (c,_) -> [(c,[])]
               | Node (_, left, right) ->
                   let leftCodes = huffmanCodes left |> List.map (fun (c, code) -> (c,1::code))
                   let rightCodes = huffmanCodes right |> List.map (fun (c, code) -> (c,0::code))
                   List.append leftCodes rightCodes
           huffmanCodes tree
           |> List.map (fun (c, code) -> (c, List.toArray code))
           |> Map.ofList
        let ptintOlolo = 
            printfn "ololo"
       
        let encode (str : string)=
            let encodeChar c =
                match huffmanCodeTable |> Map.tryFind c with
                | Some bits -> bits
                | None -> failwith "No frequency info provided for character %c" c
            str.ToCharArray()
            |> Array.map encodeChar
            |> Array.concat
        
        let decode bits =
            let rec decodeInner bitsLeft treeNode result =
                match bitsLeft, treeNode with
                | [], Node(_,_,_) -> failwith "Bits provided did not form a complete word"
                | [], Leaf(c,_) -> (c::result) |>List.rev |> List.toArray
                | _, Leaf(c,_) -> decodeInner bitsLeft tree (c::result)
                | b::rest, Node(_,l,r) -> if b
                                          then decodeInner rest l result
                                          else decodeInner rest r result
            let bitsList = Array.toList bits 
            new string (decodeInner bitsList tree [])
        
        

        
        member coder.Encode source = encode source
        member coder.Decode source = decode source

    