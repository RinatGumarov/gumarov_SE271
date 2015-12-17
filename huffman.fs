module HuffmanCompression = 
    open System.IO
    open System.Collections.Generic
// дерево
// листья дерева хранят в себе символ и сколько раз этот символ встречается(частоту)
// все остальные вершины хранят ссылки на поддеревья и сумму частот поддеревьев
    
    type Tree = 
        | Leaf of char * int
        | Node of int * Tree * Tree

    // возведение числа х в положительную степень d
    let rec degree x d =
        match d with
        |0 -> 1
        |1 -> x
        |_ -> x * (degree x (d-1))


    //перевод последовательности из 8 битов в число, напр. [0;0;0;0;0;0;1;0] -> 2
    let binToDec (bin:int list) = [for i in 0 .. 7 -> bin.Item(i)*(if i = 7
                                                                   then 1   // 2^0
                                                                   else if i = 6
                                                                        then 2
                                                                        else ([for j in 0 .. (6-i) -> 2] |> List.reduce(*)))] |> List.sum

    // перевод числа <256 в последовательность 0 и 1 двоичная запись в списке напр. 6 -> [0;0;0;0;0;1;1;0]
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
    let copy (list: int list) index count = list.GetSlice(Some(index), Some(index+count-1))
 
    // length of dec number
    let rec len x =
        match x/10 with  // делим нацело на десять -> убираем цифру с конца
        | 0 -> 1
        | _ -> 1 + (len (x/10))

    // получим чистоту из вершины
    let freq node = 
            match node with
            | Leaf(_,p) -> p
            | Node(p,_,_) -> p

            //создание дерева по списку из листьев
            //нижний уровень - листья(символ,частота)
            //сортируем элемент по частоте, чтобы выбрать два с наименьшей
            //создаем новую вершину по принципу - вершины с наименьшей частотой - сыновья, частота = сумма частот сыновей
    
    let rec buildTree roots =
            match roots |> List.sortBy freq with                                   //сортировка по частоте
            | [] -> failwith "Error! empty list"
            | [node] -> node
            | minmin::min::rest ->                                                 //берем две вершины с наименьшей частотой
                let newNode = Node(freq minmin + freq min, minmin, min)            // создаем новую вершину
                buildTree (newNode::rest)                                          // продолжаем строить дерево заменив две вершины на новую
                                                                                   // получить символ из листа
    let getCharFromLeaf node = 
        match node with 
        | Leaf(x,_)-> x 
        | Node(_,_,_)->failwith "Expected Leaf, but here is a Node"
                                                                                   // преобразовать цифру типа char в тип int
    let parseOneDigitToInt x = 
        match x with 
        |'0'->0|'1'->1|'2'->2|'3'-> 3|'4'->4|'5'->5
        |'6'->6|'7'->7|'8'->8|'9'->9|_->failwith "Error! It is not a digit"

    let rec archive (x: string) (inputPath: string) (outputPath: string)= 
        match x with
        |"-"|"c" ->                                                                     //compress 
            let input = File.ReadAllText(inputPath)                                     // считать input
            let mutable j  = 0
            let leafsDictionary = new Dictionary<char, int>()
            for i in input do
                if leafsDictionary.ContainsKey(i) then
                    leafsDictionary.[i] <- leafsDictionary.[i]+1
                    else
                    leafsDictionary.[i] <- 1
            let leafs = [for i in leafsDictionary -> Leaf(i.Key,i.Value)]
            printfn "leafs done"
            let tree = buildTree leafs
            printfn "tree built"                                                  //построить дерево из листьев
            let huffmanCodeTable =
               let rec huffmanCodes tree =
                   match tree with
                   | Leaf (c,_) -> [(c,[])]
                   | Node (_, left, right) ->
                       let leftCodes = huffmanCodes left |> List.map (fun (c, code) -> (c,1::code))  // если идем по левой ветке, то записываем 1
                       let rightCodes = huffmanCodes right |> List.map (fun (c, code) -> (c,0::code))// если идем по правой ветке, то записываем 0
                       List.append leftCodes rightCodes                                              // склеим списки из пар символ;код
               huffmanCodes tree                                                                     // построим таблицу символов
               |> List.map (fun (c, code) -> (c, List.toArray code))
               |> Map.ofList;                                                                        // преобразуем список пар в карту
            let encode (str : string) =
                let encodeChar c =                                                                   //сопоставим символу его код
                    match huffmanCodeTable |> Map.tryFind c with
                    | Some bits -> bits
                    | None -> failwith "No code provided for character"
                str.ToCharArray()
                |> Array.map encodeChar                                                              // сопоставим каждому символу входной строки его код
                |> Array.concat
                |> Array.toList
            // get bits sequence
            let listOfBits = encode input
            printfn"encoded"
            //list of encoded chars
            let listOfChars = ([for i in 0 .. ((listOfBits.Length)/8 - 1) -> 
                                copy listOfBits (i+7*i) 8]                               // список из списков бит по 8 
                                |> List.map binToDec|> List.map char ) @                 // преобразование каждой восьмерки бит в символ
                                [char(binToDec((copy listOfBits (8*(listOfBits.Length/8))// копирование остатка бит с позиции где заканчивается список из бит по 8
                                                    (listOfBits.Length-(8*(listOfBits.Length/8)))) @ //число копируемых бит
                                       [for i in 0 .. (7-(listOfBits.Length%8)) -> 0]))] // приписываем нули чтобы получить список из 8ми элементов
            printfn"all done"
            let rest0 = char (binToDec(decToBin(8-(listOfBits.Length%8))) + 100)         // количество приписанных нулей
            let str = new StreamWriter(outputPath + "output.txt")                                     // 
            str.Write(rest0)                                                             // запишем число бит, которые не нужны

            for i in 0 .. (leafs.Length-1) do           
                     str.Write(getCharFromLeaf (leafs.Item(i)))                          // запишем все символы подряд

            str.Write(getCharFromLeaf (leafs.Item(0)))                                   // запишем снова первый символ чтобы понять где остановить чтение

            for i in 0 .. (leafs.Length-1) do 
                                    str.Write(freq (leafs.Item(i)))                      // запишем частоты
                                    str.Write(" ")                                       // через пробел
            for i in listOfChars do str.Write(i)                                         // и наконец закодированный input
            str.Close()
            System.Console.WriteLine("done")
        |"+"|"d" ->                                                             // decompress
            let input' = File.ReadAllText(inputPath)                            // считаем input
            let input = input'.ToCharArray()|> Array.toList                     // разобьем input на список символов
            let mutable i = 2                                                   // введем переменную для обхода списка (0 - число лишних нулей, 1- первый символ)
            //list of chars
            let chars =(input.Item(1))::(seq{
                while not (input.Item(1).Equals(input.Item(i))) do              // считываем символы пока не встретится идентичный первому
                 yield input.Item(i)
                 i <- i+1
                }|>Seq.toList)
            i <- i+1
            let rec num (x: int list)=                                          // перевод разбитого в список по одной цифре десятичного числа в число
             match x with
             |[a] -> a 
             |[x;y] -> 10*x+y
             |(l::r::ls) -> num ((10*l+r)::ls)
             |_ -> 0
            let mutable sp = 0                                                  // введем переменную чтобы считать количество пробелов(после каждого числа ставится пробел)
            let freqs' = (seq{                                                  // чисел столько же сколько и символов
                while sp < (chars.Length) do                                    // пока количество пробелов не сравнялось с количеством символов
                    yield (seq{
                        while not (' '.Equals(input.Item(i))) do                // пока текущий элемент не является пробелом
                            yield input.Item(i)                                 // запишем элемент в список
                            i <- i + 1                                          // инкриментируем переменную обхода
                    }|> Seq.toList)|>List.map parseOneDigitToInt                // каждый символ цифры переведем в int
                    sp <- sp+1                                                  // инкриментируем кол-во пробелов
                    i<-i+1                                                      // инкрементиируем переменную обхода
                } |> Seq.toList) |> List.map num                                // из списка списков цифр сделаем список чисел
            // lest of frequences

            // list of leafs
            let leafs = List.zip chars freqs' |> List.map (fun (x,y)->Leaf(x,y))// из полученных последовательностей частот и символов соберем листья
            let tree = buildTree leafs                                          // теперь из листьев восстановим дерево
            let decode (bits: int list) =
                let rec decodeInner bitsLeft treeNode result =
                    match bitsLeft, treeNode with                             // смотрим оставшуюся последовательность из 0 и 1 и вершину дерева в которой сейчас находимся
                    | [], Node(_,_,_) -> result |>List.rev |> List.toArray    // если вдруг произошла ошибка и мы не дошли до листа, то покажем то что получилось
                    | [], Leaf(c,_) -> (c::result) |>List.rev |> List.toArray // мы дошли до конца в последовательности и находимся в листе. пишем символ и все ок.
                    | _, Leaf(c,_) -> decodeInner bitsLeft tree (c::result)   // последовательность еще не закончилась и мы уже в листе, начинаем снова с корня
                    | b::rest, Node(_,l,r) -> if (b=1)                        // биты еще не закончились и мы находимся в вершине дерева
                                              then decodeInner rest l result  // если записана 1, то мы идем в левое поддерево и читаем биты дальше
                                              else decodeInner rest r result  // а если 0, то в правое
                new string (decodeInner bits tree [])                         // запишем результат в строку.

            let delete = int(input.Item(0))-100                                //bits that should be deleted 
            let bufBitSeq = [for x in (i) .. (input.Length-1) ->               //(100 - просто число которое мы прибавили при сжатии чтобы получился символ)
                               (decToBin (int (input.Item(x))))] |> List.concat// переведем каждый символ прочтенной строки сначала в число а потов в список из 0 и 1
            let bitSeq = copy bufBitSeq 0 (bufBitSeq.Length-delete)            // скопируем только нужную часть без лишних нулей в конце
            let str = new StreamWriter(outputPath + "/output.trn")
            for c in (decode bitSeq) do str.Write(c)                           // выведем результат в файл
            str.Close()
            System.Console.WriteLine("done")
        | _ ->
            printfn"use:: compress or decompress? <c/d> or <-/+>"

    [<EntryPoint>]
    let main argv = 
        let argList = argv |> List.ofSeq
        match argList with
        |[fst;snd;thr]-> archive fst snd thr
        |[]->printfn"nothing. use:: compress or decompress? <c/d> or <-/+>"
        |_-> printfn"oops. something goes wrong. use:: compress or decompress? <c/d> or <-/+>"
        0