module HuffmanCompression = 
    open System.IO
// дерево
// листья дерева хранят в себе символ и сколько раз этот символ встречается(частоту)
// все остальные вершины хранят ссылки на поодеревья и сумму частот поддеревьев
    
    type Tree = 
        | Leaf of char * int
        | Node of int * Tree * Tree
    printfn"ololol"
    let rec degree x d =
        match d with
        |0 -> 1
        |1 -> x
        |_ -> x * (degree x (d-1))
    let s = "8478ki"
    let g = s.

    let input = File.ReadAllText("/Users/Rinat/Movies/test.txt")
//    let input = "1992 Nintendo"
    printfn "%A" input

    let listOfFreqs = [for i in input -> (i, 1)]

    //перевод последовательности из 8 битов в число
    let binToDec (bin:int list) = [for i in 0 .. 7 -> bin.Item(i)*(if i = 7 then 1 else ([for j in 0 .. (6-i) -> 2] |> List.reduce(*)))] |> List.reduce(+)

    let decToBin  (b:int) = 
        let rec dtb a =
            match a/2 with
            | 0 -> [a%2]
            | _ -> a%2 :: (dtb (a/2))
        let rec add0 (a:int list) = 
            match a.Length with
            | 8 -> a
            | _ -> add0 (0::a)
        add0 (List.rev (dtb b))

    //копирование count элементов списка list начиная с index
    let copy (list: int list) index count = [for i in index .. (index + count - 1) -> list.Item(i)]

    //убрать повторяющиеся элементы списка, оставив только первое вхождение (повторяются только символы, частоты могут быть разными)
    let rec otsev = function
        | (p::xs) -> p::otsev [ for x in xs do if fst x <> fst p then yield x ]
        | [] -> []

    //при каждой следующей встрече символа, увеличим его частоту на единицу
    let rec otsev2 = function
        | (p::xs) -> p::otsev2 [ for x in xs do if fst x = fst p then yield (fst x, snd p + 1)
                                                else yield x ]
        | [] -> []
      
    //получим список из пар (символ,частота) -> список из листьев
    let leafs = otsev (List.rev (otsev2 listOfFreqs)) |> List.map (fun (x,y)->Leaf(x,y))

    let rec len x =
        match x/10 with
        | 0 -> 1
        | _ -> 1 + (len (x/10))

   // getFreq
    let freq node = 
            match node with
            | Leaf(_,p) -> p
            | Node(p,_,_) -> p
//    let chr node = match
         

            //создание дерева по списку из листьев
            //нижний уровень - листья(символ,частота)
            //сортируем элемент по частоте, чтобы выбрать два с наименьшей
            //создаем новую вершину по принципу - вершины с наименьшей частотой - сыновья, частота = сумма частот сыновей
    
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
           |> Map.ofList;
    
    
    let encode (str : string) =
            let encodeChar c =
                match huffmanCodeTable |> Map.tryFind c with
                | Some bits -> bits
                | None -> failwith "No frequency info provided for character %c" c
            str.ToCharArray()
            |> Array.map encodeChar
            |> Array.concat
            |> Array.toList

    // получим последовательность из нулей и единиц
    printfn "adin"
    let listOfBits = encode input
    printfn"iii"
    printfn"%A" listOfBits
    printfn"printed"

    // получим последовательность из символов отрезав несколько бит с конца, чтобы делилось на 8
    let listOfChars = ([for i in 0 .. ((listOfBits.Length)/8 - 1) -> copy listOfBits (i+7*i) 8]|> List.map binToDec|> List.map char ) @ [char(binToDec ((copy listOfBits (8*(listOfBits.Length/8)) (listOfBits.Length-(8*(listOfBits.Length/8))-1)) @ [for i in 0 .. listOfBits.Length%8 -> 0]))]
    printfn "adin %A" listOfChars

    let str = new StreamWriter("/Users/Rinat/FSProjects/Huffman/output.ri")
    for i in listOfChars do str.Write(i)
    str.Close();

    let bitiki = File.ReadAllText("/Users/Rinat/FSProjects/Huffman/output.ri")|> Seq.toList |> List.map int

    let bitbit = [for i in bitiki -> (decToBin i)] |> List.concat

//    File.WriteAllText("output.trn", (encode input))
    
      // восстановить исходный файл по списку из нулей и единиц  
    let decode bits = 
            let rec decodeInner bitsLeft treeNode result =
                match bitsLeft, treeNode with
                | [], Node(_,_,_) -> failwith "Bits provided did not form a complete word"
                | [], Leaf(c,_) -> (c::result) |>List.rev |> List.toArray
                | _, Leaf(c,_) -> decodeInner bitsLeft tree (c::result)
                | b::rest, Node(_,l,r) -> if (b=1)
                                          then decodeInner rest l result
                                          else decodeInner rest r result 
            new string (decodeInner bits tree [])  
    printfn "decode %A" (decode (bitbit@[0;0;0]))

    let rec archive (inputPath: string) (outputPath:string) (x: char) = 
        match x with
        |'-' -> 
            let input = File.ReadAllText(inputPath)
            let leafs = otsev (List.rev (otsev2 ([for i in input -> (i, 1)]))) |> List.map (fun (x,y)->Leaf(x,y))
            let tree = buildTree leafs
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
               |> Map.ofList;
            let encode (str : string) =
                let encodeChar c =
                    match huffmanCodeTable |> Map.tryFind c with
                    | Some bits -> bits
                    | None -> failwith "No frequency info provided for character %c" c
                str.ToCharArray()
                |> Array.map encodeChar
                |> Array.concat
                |> Array.toList
            let listOfBits = encode input
            let listOfChars = ([for i in 0 .. ((listOfBits.Length)/8 - 1) -> copy listOfBits (i+7*i) 8]|> List.map binToDec|> List.map char ) @ [char(binToDec ((copy listOfBits (8*(listOfBits.Length/8)) (listOfBits.Length-(8*(listOfBits.Length/8))-1)) @ [for i in 0 .. listOfBits.Length%8 -> 0]))]
            let str = new StreamWriter(outputPath)

            str.Write(([for i in leafs -> len (freq i)]|> List.reduce (+)) + (leafs.Length * 2))
            str.Write('_')
            for i in leafs do 
                str.Write((fun node->match node with | Leaf(x,_)-> x | Node(_,_,_)->failwith "Expected Leaf, but here Node") i)
                str.Write(len (freq i))
                str.Write(freq i)
            for i in listOfChars do str.Write(i)
            str.Close()
        |'+' -> 
//            let readFile (s:char list) =
//                let mutable i = -1
//                let parse c =
//                    match c with
//                    |'0' -> 0
//                    |'1' -> 1
//                    |'2' -> 2
//                    |'3' -> 3
//                    |'4' -> 4
//                    |'5' -> 5
//                    |'6' -> 6
//                    |'7' -> 7
//                    |'8' -> 8
//                    |'9' -> 9
//                    | _ -> failwith "couldnt parse"
//                let rec length l =
//                    i <- i+1
//                    match s.Item(i) with
//                    |'_' -> []
//                            i <- i+1
//                    | _  -> (parse (s.Item(i)))::(length l)
//                let l = length s 
//                for i in 0 .. l.Length -> l.Item(i)*(degree 10 (l.Length - i))
//                printfn "%A" l
//                let
//

            let input = File.ReadAllText(inputPath)|> Seq.toList |> List.map int
            let decode bits = 
                let rec decodeInner bitsLeft treeNode result =
                    match bitsLeft, treeNode with
                    | [], Node(_,_,_) -> failwith "Bits provided did not form a complete word"
                    | [], Leaf(c,_) -> (c::result) |>List.rev |> List.toArray
                    | _, Leaf(c,_) -> decodeInner bitsLeft tree (c::result)
                    | b::rest, Node(_,l,r) -> if (b=1)
                                              then decodeInner rest l result
                                              else decodeInner rest r result 
                new string (decodeInner bits tree []) 
            let bitbit = [for i in input -> (decToBin i)] |> List.concat
            let str = new StreamWriter(outputPath)
            for i in (decode (bitbit@[0;0;0])) do str.Write(i)
            str.Close()
        | _ -> printfn"use:: inputPath outputPath x(+ -> decompress; - -> compress)" 
    archive "/Users/Rinat/Movies/test.txt" "/Users/Rinat/Movies/test33.txt" '-'

