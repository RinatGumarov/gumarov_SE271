module HuffmanCompression = 
    open System.IO
// дерево
// листья дерева хранят в себе символ и сколько раз этот символ встречается(частоту)
// все остальные вершины хранят ссылки на поодеревья и сумму частот поддеревьев
    
    type Tree = 
        | Leaf of char * int
        | Node of int * Tree * Tree

//    let input = File.ReadAllText("input.txt")
    let input = "olol fgbbeyt tynr tyumr umry mruymr yo"
   
    let listOfFreqs = seq[for i in input -> (i, 1)] |> Seq.toList

    //перевод последовательности из 8 битов в число
    let binToDec (bin:int list) = [for i in 0 .. 7 -> bin.Item(i)*(if i = 7 then 1 else ([for j in 0 .. (6-i) -> 2] |> List.reduce(*)))] |> List.reduce(+)

    //копирование count элементов списка list начиная с index
    let copy (list: int list) index count = [for i in index .. (index + count - 1) -> list.Item(i)]

    //убрать повторяющиеся элементы списка, оставив только первое вхождение (повторяются только символы, частоты могуут быть разными)
    let rec otsev = function
        | (p::xs) -> p::otsev [ for x in xs do if fst x <> fst p then yield x ]
        | [] -> []

    //развернуть список
    let reverse list = List.fold(fun acc x -> x::acc) [] list

    //при каждой следующей встрече символа, увеличим его частоту на единицу
    let rec otsev2 = function
        | (p::xs) -> p::otsev2 [ for x in xs do if fst x = fst p then yield (fst x, snd p + 1)
                                                else yield x ]
        | [] -> []
    
    //получим список из пар (символ,частота) -> список из листьев
    let leafs = otsev (reverse (otsev2 listOfFreqs)) |> List.map Leaf
    
   
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
    let listOfBits = encode input

    // получим последовательность из символов отрезав несколько бит с конца, чтобы делилось на 8
    let listOfChars = ([for i in 0 .. (((encode input).Length)/8 - 1) -> copy listOfBits i 8]|> List.map binToDec|> List.map char )
    let last8 = (binToDec ((copy listOfBits (8*(listOfBits.Length/8)) (listOfBits.Length-(8*(listOfBits.Length/8))-1)) @ [for i in 0 .. listOfBits.Length%8 -> 0]))
//    File.WriteAllText("output.trn", (encode input))
    
      // восстановить исходный файл по списку из нулей и единиц  
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