module HuffmanCompression = 
    open System.IO
// дерево
// листья дерева хранят в себе символ и сколько раз этот символ встречается(частоту)
// все остальные вершины хранят ссылки на поодеревья и сумму частот поддеревьев
    
    type Tree = 
        | Leaf of char * int
        | Node of int * Tree * Tree

    let sw = new StreamReader("/Users/Rinat/FSProjects/Huffman/one.txt")
    let input = File.ReadAllText("/Users/Rinat/FSProjects/Huffman/one.txt")
    sw.Close()
    printfn "%A" input
//    let input = File.ReadAllText("input.txt")
//    let input = "olol fgbbeyt tynr tyumr umry mruymr yo"
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
    printfn"%A" (decToBin 4)

    printfn "%A" 65

    //копирование count элементов списка list начиная с index
    let copy (list: int list) index count = [for i in index .. (index + count - 1) -> list.Item(i)]

    //убрать повторяющиеся элементы списка, оставив только первое вхождение (повторяются только символы, частоты могуут быть разными)
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
    printfn "%A" leafs

    
   
   // getFreq
    let freq node = 
            match node with
            | Leaf(_,p) -> p
            | Node(p,_,_) -> p


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
    printfn"%A" tree

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

    printfn "%A" huffmanCodeTable
    
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
    let listOfBits = encode input

    // получим последовательность из символов отрезав несколько бит с конца, чтобы делилось на 8
    let listOfChars = ([for i in 0 .. ((listOfBits.Length)/8 - 1) -> copy listOfBits i 8]|> List.map binToDec|> List.map char ) @ [char(binToDec ((copy listOfBits (8*(listOfBits.Length/8)) (listOfBits.Length-(8*(listOfBits.Length/8))-1)) @ [for i in 0 .. listOfBits.Length%8 -> 0]))]


    let str = new StreamWriter("/Users/Rinat/FSProjects/Huffman/output.ri")
    for i in listOfChars do str.Write(i)
    str.Close();

    let bitiki = File.ReadAllText("/Users/Rinat/FSProjects/Huffman/output.ri")|> Seq.toList |> List.map int

    printfn"bitiki %A" bitiki
    let bitbit = [for i in bitiki -> (decToBin i)] |> List.concat
    let ik = [for i in 0 .. listOfBits.Length%8 -> 0]
    printfn "%A" ik.Length
    printfn "bitbit %A" bitbit
    printfn"bitbit ff %A" listOfBits
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
                                                   
    printfn"%A" (decode (listOfBits))

    printfn "%A" (decode (bitbit@[0;0]))