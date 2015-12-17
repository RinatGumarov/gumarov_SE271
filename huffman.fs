module HuffmanCompression = 
    open System.IO
    open System.Collections.Generic
// tree
// leafs contains char and frequency of this char
// Nodes contais sum of frequencec of theirs children and theirs children
    
    type Tree = 
        | Leaf of char * int
        | Node of int * Tree * Tree

    // exponentiation x to positive degree d
    let rec degree x d =
        match d with
        |0 -> 1
        |1 -> x
        |_ -> x * (degree x (d-1))


    //translate list of 8 bits to number, example [0;0;0;0;0;0;1;0] -> 2
    let binToDec (bin:int list) = [for i in 0 .. 7 -> bin.Item(i)*(if i = 7
                                                                   then 1   // 2^0
                                                                   else if i = 6
                                                                        then 2
                                                                        else ([for j in 0 .. (6-i) -> 2] |> List.reduce(*)))] |> List.sum

    //translate number<256 into list of bits binary form. example 6 -> [0;0;0;0;0;1;1;0]
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

<<<<<<< HEAD
    //copying count elements from list begin from index
=======
    //копирование count элементов списка list начиная с index
>>>>>>> 7fb93a6ae1c9208b75529a4fea9c74a1c34f828e
    let copy (list: int list) index count = list.GetSlice(Some(index), Some(index+count-1))
 
    // length of dec number
    let rec len x =
        match x/10 with  // because it decimal
        | 0 -> 1
        | _ -> 1 + (len (x/10))

    // get frequency of node
    let freq node = 
            match node with
            | Leaf(_,p) -> p
            | Node(p,_,_) -> p

            //creating tree with list of leafs
            //button - leafs(charackter*integer)
            //sorting elements with frequency, then choose 2 minimums
            //creating new node - 2 minimums - children, frequency=(freq child1) + (freq child2)
    
    let rec buildTree roots =
            match roots |> List.sortBy freq with                                   //sortinf with frequency
            | [] -> failwith "Error! empty list"
            | [node] -> node
            | minmin::min::rest ->                                                 //choose 2 nodes with minimum freqs
                let newNode = Node(freq minmin + freq min, minmin, min)            //creating new node
                buildTree (newNode::rest)                                          //continue with new node
                                                                                   // get char from Leaf
    let getCharFromLeaf node = 
        match node with 
        | Leaf(x,_)-> x 
        | Node(_,_,_)->failwith "Expected Leaf, but here is a Node"
                                                                                   // parse digit to integer
    let parseOneDigitToInt x = 
        match x with 
        |'0'->0|'1'->1|'2'->2|'3'-> 3|'4'->4|'5'->5
        |'6'->6|'7'->7|'8'->8|'9'->9|_->failwith "Error! It is not a digit"

    let rec archive (x: string) (inputPath: string) (outputPath: string)= 
        match x with
        |"-"|"c" ->                                                                     //compress 
            let input = File.ReadAllText(inputPath)                                     //read input
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
            printfn "tree built"                                                  //create tree from leafs
            let huffmanCodeTable =
               let rec huffmanCodes tree =
                   match tree with
                   | Leaf (c,_) -> [(c,[])]
                   | Node (_, left, right) ->
                       let leftCodes = huffmanCodes left |> List.map (fun (c, code) -> (c,1::code))  // left branch -> add 1
                       let rightCodes = huffmanCodes right |> List.map (fun (c, code) -> (c,0::code))// right branch -> add 0
                       List.append leftCodes rightCodes                                              // append lists
               huffmanCodes tree                                                                     // create huffman code table
               |> List.map (fun (c, code) -> (c, List.toArray code))
               |> Map.ofList;                                                                        // list to map
            let encode (str : string) =
                let encodeChar c =                                                                   //replace char with its code
                    match huffmanCodeTable |> Map.tryFind c with
                    | Some bits -> bits
                    | None -> failwith "No code provided for character"
                str.ToCharArray()
                |> Array.map encodeChar                                                              // replace all chars in array with its codes
                |> Array.concat
                |> Array.toList
            // get bits sequence
            let listOfBits = encode input
            printfn"encoded"
            //list of encoded chars
            let listOfChars = ([for i in 0 .. ((listOfBits.Length)/8 - 1) -> 
                                copy listOfBits (i+7*i) 8]                               // list of list of 8 bits 
                                |> List.map binToDec|> List.map char ) @                 // translate list of bit to decimal
                                [char(binToDec((copy listOfBits (8*(listOfBits.Length/8))// copying rest bits
                                                    (listOfBits.Length-(8*(listOfBits.Length/8)))) @ //count of copying bits
                                       [for i in 0 .. (7-(listOfBits.Length%8)) -> 0]))] // add 0s to get list with 8 bits
            printfn"all done"
            let rest0 = char (binToDec(decToBin(8-(listOfBits.Length%8))) + 100)         // count of rest 0s
            let str = new StreamWriter(outputPath + "output.txt")                                     // 
            str.Write(rest0)                                                             // write count of bits that we should delete

            for i in 0 .. (leafs.Length-1) do           
                     str.Write(getCharFromLeaf (leafs.Item(i)))                          // write all chars

            str.Write(getCharFromLeaf (leafs.Item(0)))                                   // write first symbol so we can stop reading on it

            for i in 0 .. (leafs.Length-1) do 
                                    str.Write(freq (leafs.Item(i)))                      // write frequences
                                    str.Write(" ")                                       // with space
            for i in listOfChars do str.Write(i)                                         // and encoded input
            str.Close()
            System.Console.WriteLine("done")
        |"+"|"d" ->                                                             // decompress
            let input' = File.ReadAllText(inputPath)                            // read input
            let input = input'.ToCharArray()|> Array.toList                     // put input to list of chars
            let mutable i = 2                                                   // variable to iterate read
            //list of chars
            let chars =(input.Item(1))::(seq{
                while not (input.Item(1).Equals(input.Item(i))) do              // read chars while it is not equal to first char
                 yield input.Item(i)
                 i <- i+1
                }|>Seq.toList)
            i <- i+1
            let rec num (x: int list)=                                          // concat digits to number
             match x with
             |[a] -> a 
             |[x;y] -> 10*x+y
             |(l::r::ls) -> num ((10*l+r)::ls)
             |_ -> 0
            let mutable sp = 0                                                  // variable to count spaces
            let freqs' = (seq{                                                  // count of numbers and chars are equal
                while sp < (chars.Length) do                                    // while spaces count not equal to count of chars
                    yield (seq{
                        while not (' '.Equals(input.Item(i))) do                // while current element is not equal to space
                            yield input.Item(i)                                 // yield this element to list
                            i <- i + 1                                          // increment i
                    }|> Seq.toList)|>List.map parseOneDigitToInt                // parse every char of digit to digit
                    sp <- sp+1                                                  // incremeent count of spaces
                    i<-i+1                                                      // increment i
                } |> Seq.toList) |> List.map num                                // making list of numbers from list of list of digits
            // lest of frequences

            // list of leafs
            let leafs = List.zip chars freqs' |> List.map (fun (x,y)->Leaf(x,y))// make leafs from lists of freqs and chars
            let tree = buildTree leafs                                          // now build tree with our leafs
            let decode (bits: int list) =
                let rec decodeInner bitsLeft treeNode result =
                    match bitsLeft, treeNode with                             // 
                    | [], Node(_,_,_) -> result |>List.rev |> List.toArray    // 
                    | [], Leaf(c,_) -> (c::result) |>List.rev |> List.toArray // if sequence  is empty and we are in leaf it is ok!
                    | _, Leaf(c,_) -> decodeInner bitsLeft tree (c::result)   // seqquence is not empty and we are in leaf -> begin again from root
                    | b::rest, Node(_,l,r) -> if (b=1)                        // bits are not end and we are in node
                                              then decodeInner rest l result  // if there is 1, we go to the left tree
                                              else decodeInner rest r result  // if 0, to the right
                new string (decodeInner bits tree [])                         // write result to string

            let delete = int(input.Item(0))-100                                //bits that should be deleted 
            let bufBitSeq = [for x in (i) .. (input.Length-1) ->               //(100 - is simple number that we are chose when write)
                               (decToBin (int (input.Item(x))))] |> List.concat// translate every symbol to number and the to its binary view
            let bitSeq = copy bufBitSeq 0 (bufBitSeq.Length-delete)            // copying without rest of 0s
            let str = new StreamWriter(outputPath + "/output.trn")
            for c in (decode bitSeq) do str.Write(c)                           // write result to file
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