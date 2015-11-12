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
