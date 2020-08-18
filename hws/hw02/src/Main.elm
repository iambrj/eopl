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

treeMap : (Int -> Int) -> Tree -> Tree
treeMap f t =
    case t of
        Leaf i -> Leaf (f i)
        Node i l r -> Node (f i) (treeMap f l) (treeMap f r)

type PathItem = Left | Right

value : Tree -> Int
value t =
    case t of
        Leaf v -> v
        Node v l r -> v

subtree : PathItem -> Tree -> Maybe Tree
subtree p t = case t of
        Leaf _ -> Nothing
        Node _ l r -> if p == Left then Just l else Just r

value_at_path : List PathItem -> Tree -> Maybe Int
value_at_path p tree =
    case p of
        [] -> Just (value tree)
        (h :: t) -> case h of
            Left -> case (subtree Left tree) of
                Just l -> value_at_path t l
                Nothing -> Nothing
            Right -> case (subtree Right tree) of
                Just r -> value_at_path t r
                Nothing -> Nothing

altor : Maybe a -> Maybe a -> Maybe a
altor x y = case x of
    Just _ -> x
    _ -> y

combine : Maybe (List t) -> Maybe (List t) -> Maybe (List t)
combine l1 l2 =
    case (l1, l2) of
        (Just ll1, Just ll2) -> Just (ll1 ++ ll2)
        (_, _) -> Nothing

search : Int -> Tree -> Maybe (List PathItem)
search x t = case t of
    Leaf v -> if v == x then Just [] else Nothing
    Node v l r -> if v == x
        then Just []
        else altor (combine (Just [Left]) (search x l)) (combine (Just [Right]) (search x r))

updateVal f t =
    case t of
        Leaf v -> Leaf (f v)
        Node v l r -> Node (f v) l r

update : List PathItem -> (Int -> Int) -> Tree -> Tree
update p f t =
    case p of
        [] -> updateVal f t
        (h::hs) -> case h of
            Left -> case (subtree Left t) of
                Just l -> update hs f l
                Nothing -> t
            Right -> case (subtree Right t) of
                Just r -> update hs f r
                Nothing -> t

tree_insert : List PathItem -> Tree -> Tree -> Tree -> Tree
tree_insert p l r t = case p of
    [] -> case t of
        Leaf v -> Node v l r
        Node _ _ _ -> t
    h::hs -> case (subtree h t) of
        Nothing -> t
        Just s -> tree_insert hs l r s
