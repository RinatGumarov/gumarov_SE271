module HuffmanCompression = 
    open System.IO
// дерево
// листья дерева хранят в себе символ и сколько раз этот символ встречается(частоту)
// все остальные вершины хранят ссылки на поодеревья и сумму частот поддеревьев
    
    type Tree = 
        | Leaf of char * int
        | Node of int * Tree * Tree

    let rec degree x d =
        match d with
        |0 -> 1
        |1 -> x
        |_ -> x * (degree x (d-1))


    //перевод последовательности из 8 битов в число

    let binToDec (bin:int list) = [for i in 0 .. 7 -> bin.Item(i)*(if i = 7 then 1 else if i = 6 then 2 else ([for j in 0 .. (6-i) -> 2] |> List.reduce(*)))] |> List.sum

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
     

    //убрать повторяющиеся элементы списка, оставив только первое вхождение (повторяются только символы, частоты могуут быть разными)
    let rec otsev = function
        | (p::xs) -> p::otsev [ for x in xs do if fst x <> fst p then yield x ]
        | [] -> []

    //при каждой следующей встрече символа, увеличим его частоту на единицу
    let rec otsev2 = function
        | (p::xs) -> p::otsev2 [ for x in xs do if fst x = fst p then yield (fst x, snd p + 1)
                                                else yield x ]
        | [] -> []

    // length of number
    let rec len x =
        match x/10 with
        | 0 -> 1
        | _ -> 1 + (len (x/10))

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
        
    let rec archive (x: int) = 
        System.Console.Write("enter input:")
        match x with
        |45|99 -> 
            let input = System.Console.ReadLine()
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
            // get bits sequence
            let listOfBits = encode input
            //list of encoded chars
            let listOfChars = ([for i in 0 .. ((listOfBits.Length)/8 - 1) -> copy listOfBits (i+7*i) 8]|> List.map binToDec|> List.map char ) @ [char(binToDec ((copy listOfBits (8*(listOfBits.Length/8)) (listOfBits.Length-(8*(listOfBits.Length/8)))) @ [for i in 0 .. (7-(listOfBits.Length%8)) -> 0]))]
            let rest0 = char (binToDec(decToBin(8-(listOfBits.Length%8))) + 100)
            System.Console.Write(rest0) // количество последних лишних нулей
            for i in 0 .. (leafs.Length-1) do System.Console.Write((fun node->match node with | Leaf(x,_)-> x | Node(_,_,_)->failwith "Expected Leaf, but here Node") (leafs.Item(i)))//символы подряд
            System.Console.Write((fun node->match node with | Leaf(x,_)-> x | Node(_,_,_)->failwith "Expected Leaf, but here Node") (leafs.Item(0)))
            for i in 0 .. (leafs.Length-1) do 
                                    System.Console.Write((fun node->match node with | Leaf(_,x)-> x | Node(_,_,_)->failwith "Expected Leaf, but here Node") (leafs.Item(i)))
                                    System.Console.Write(" ")
            for i in listOfChars do System.Console.Write(i)
            //write to file
//            let str = new StreamWriter("/Users/Rinat/Movies/ouytput.txt")
//            str.Write(rest0)
//            for i in 0 .. (leafs.Length-1) do str.Write((fun node->match node with | Leaf(x,_)-> x | Node(_,_,_)->failwith "Expected Leaf, but here Node") (leafs.Item(i)))//символы подряд
//            str.Write((fun node->match node with | Leaf(x,_)-> x | Node(_,_,_)->failwith "Expected Leaf, but here Node") (leafs.Item(0)))
//            for i in 0 .. (leafs.Length-1) do 
//                                    str.Write((fun node->match node with | Leaf(_,x)-> x | Node(_,_,_)->failwith "Expected Leaf, but here Node") (leafs.Item(i)))
//                                    str.Write(" ")
//            for i in listOfChars do str.Write(i)
//            str.Close()
        |43|100 -> 
            let input' = System.Console.ReadLine()
            let input = input'.ToCharArray()|> Array.toList
            let mutable i = 2
            //list of chars
            let chars =(input.Item(1))::(seq{
                while not (input.Item(1).Equals(input.Item(i))) do
                 yield input.Item(i)
                 i <- i+1
                }|>Seq.toList)
            i <- i+1
            let rec num (x: int list)=
             match x with
             |[a] -> a 
             |[x;y] -> 10*x+y
             |(l::r::ls) -> num ((10*l+r)::ls)
             |_ -> 0
            let mutable sp = 0
            let freqs = (seq{
                while sp < (chars.Length - 1) do
                    yield (seq{
                        while not (' '.Equals(input.Item(i))) do
                            yield input.Item(i)
                            printfn "%A" (input.Item(i))
                            i <- i + 1
                    }|> Seq.toList)|>List.map (fun x -> match x with |'0'->0|'1'->1|'2'->2|'3'-> 3|'4'->4|'5'->5|'6'->6|'7'->7|'8'->8|'9'->9|_->0)
                    sp <- sp+1
                    i<-i+1
                }|> Seq.toList) |> List.map num 
            // lest of frequences
            let freqs' = freqs @ [(num [(fun x -> match x with |'0'->0|'1'->1|'2'->2|'3'-> 3|'4'->4|'5'->5|'6'->6|'7'->7|'8'->8|'9'->9|_->0) (input.Item(i))])]
            // list of leafs
            let leafs = List.zip chars freqs' |> List.map (fun (x,y)->Leaf(x,y))
            let tree = buildTree leafs
            let decode bits = 
                let rec decodeInner bitsLeft treeNode result =
                    match bitsLeft, treeNode with
                    | [], Node(_,_,_) -> result |>List.rev |> List.toArray 
                    | [], Leaf(c,_) -> (c::result) |>List.rev |> List.toArray
                    | _, Leaf(c,_) -> decodeInner bitsLeft tree (c::result)
                    | b::rest, Node(_,l,r) -> if (b=1)
                                              then decodeInner rest l result
                                              else decodeInner rest r result 
                new string (decodeInner bits tree []) 
            let delete = int(input.Item(0))-100 //bits that should be deleted
            let bufBitSeq = [for x in (i+2) .. (input.Length-1) -> (decToBin (int (input.Item(x))))] |> List.concat
            let bitSeq = copy bufBitSeq 0 (bufBitSeq.Length-delete)
            for c in (decode bitSeq) do System.Console.Write(c)
        | _ ->

            printfn"use:: compress or decompress? <c/d> or <-/+>"
            let var = System.Console.Read()
            System.Console.WriteLine()
            archive  var
            
    
//    printfn"use:: compress or decompress? <c/d> or <-/+>"
//    let var = System.Console.Read()
//    System.Console.WriteLine()
//    archive  var
    open System
    open System.Text
    // Create two different encodings.
    let ascii = Encoding.ASCII;
    let unicode = Encoding.Unicode;

    // Convert the string into a byte array.
    let unicodeBytes = unicode.GetBytes("olololololo");
    System.Console.Write(unicodeBytes)
    printfn "%A" unicodeBytes

    // Perform the conversion from one encoding to the other.
    let asciiBytes = Encoding.Convert(unicode, ascii, unicodeBytes);

    let asciiChars' = ascii.GetChars(asciiBytes)
    let asciiChars = new string (asciiChars')
    printfn "%A" asciiChars
    System.Console.Write(asciiChars)

//    string asciiString = new string(asciiChars);
