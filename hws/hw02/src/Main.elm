module Main exposing (..)

repeat n x =
    if n == 0 then [] else x :: repeat (n - 1) x

invertList l =
    case l of
        (h :: t) -> invertList t ++ [h]
        []       -> []

invert l =
    case l of
        (h :: t) -> invertList h :: invert t
        []       -> []

count_occurrences : t -> List t -> Int
count_occurrences s slist =
    case slist of
        (h :: t) -> if (h == s) then 1 + (count_occurrences s t) else (count_occurrences s t)
        []       -> 0

product l1 l2 =
    case (l1, l2) of
        (h1 :: t1, h2 :: t2) -> List.map (\x -> [h1, x]) (h2 :: t2) ++ (product t1 (h2 :: t2))
        _                    -> []

every pred l =
    case l of
        h :: t -> if not (pred h) then False else every pred t
        []     -> True

merge l1 l2 =
    case (l1, l2) of
        (h1::t1, h2::t2) -> if h1 < h2 then h1 :: merge t1 l2 else h2 :: merge l1 t2
        ([], _) -> l2
        (_, _) -> l1

type Tree = Leaf Int | Node Int Tree Tree

singleton_tree = Leaf 10

regular_tree =
    Node 100
         (Node 50
               (Node 200
                     (Leaf 5)
                     (Leaf 6))
               (Leaf 80))
         (Leaf 40)

preorder t =
    case t of
        Leaf i -> [i]
        Node i l r -> [i] ++ preorder l ++ preorder r

inorder t =
    case t of
        Leaf i -> [i]
        Node i l r -> inorder l ++ [i] ++ inorder r

postorder t =
    case t of
        Leaf i -> [i]
        Node i l r -> postorder l ++ postorder r ++ [i]

count_nodes t =
    case t of
        Leaf i -> 1
        Node i l r -> 1 + count_nodes l + count_nodes r

count_leaves t =
    case t of
        Leaf i -> 1
        Node i l r -> count_leaves l + count_leaves r

count_internal t =
    case t of
        Leaf i -> 0
        Node i l r -> 1 + count_internal l + count_internal r

treeMap f t =
    case t of
        Leaf i -> Leaf (f i)
        Node i l r -> Node (f i) (treeMap f l) (treeMap f r)
