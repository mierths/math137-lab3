module Lib.List exposing (..)

{-| Extra functions to make working with lists easier.

# Create
@docs iterate, unfold, fromMaybe

# Transform
@docs dropWhile, takeWhile, filterMaybes, traverse

# Combine
@docs zip, cross, join

# Deconstruct
@docs inc, dec, find, mapReduce, groupBy

-}

import Debug exposing (..)
import Dict exposing (Dict)


{-| A less-general form of `unfold`. Create a list by repeatedly applying
a function, stopping when the function returns `Nothing`.

**Warning:** This function can cause an infinite loop (or, more likely,
a "Maximum call stack size exceeded" error).

    iterate (\x -> if x == 3 then Nothing else Just (x + 1)) 0 == [1, 2, 3]
-}
iterate : (a -> Maybe a) -> a -> List a
iterate f = unfold (\x -> f x |> Maybe.map (\y -> (y, y)))


{-| Create a list by repeatedly applying a function, stopping when the
function return `Nothing`.

**Warning:** This function can cause an infinite loop (or, more likely,
a "Maximum call stack size exceeded" error).

    unfold (\x -> if x > 3 then Nothing else Just (x, x + 1)) 1 == [1, 2, 3]
-}
unfold : (b -> Maybe (a, b)) -> b -> List a
unfold f b0 =
  case f b0 of
    Nothing -> []
    Just (a, b1) -> a :: unfold f b1


{-| Create either a singleton list or an empty list from a `Maybe`.

    fromMaybe Nothing  == []
    fromMyabe (Just x) == [x]
-}
fromMaybe : Maybe a -> List a
fromMaybe m =
  case m of
    Nothing -> []
    Just x -> [x]


{-| Drop elements from the front of a list until the supplied callback is false.

    dropWhile (\x -> x < 5) [1, 2, 3, 4, 5, 3] == [5, 3]
-}
dropWhile : (a -> Bool) -> List a -> List a
dropWhile p list =
  case list of
    [] -> []
    x :: xs -> if p x then dropWhile p xs else x :: xs


{-| Take elements from the front of a list until the supplied callback is false.

    takeWhile (\x -> x < 5) [1, 2, 3, 4, 5, 3] == [1, 2, 3, 4]
-}
takeWhile : (a -> Bool) -> List a -> List a
takeWhile p list =
  let
    helper acc rest =
      case rest of
        [] -> acc
        x :: xs -> if p x then helper (x :: acc) xs else acc
  in
  List.reverse (helper [] list)


{-| Filter `Nothing`s out of a list of `Maybe`s, unwrappig the `Just`s.

    filterMaybes [Just 1, Just 2, Nothing, Just 4] == [1, 2, 4]
-}
filterMaybes : List (Maybe a) -> List a
filterMaybes = List.filterMap identity


{-| Create a list of pairs by lining two lists up.
If one list is longer, the extra elements are dropped.

    zip [1, 2, 3] ["a", "b", "c", "d"] == [(1, "a"), (2, "b"), (3, "c")]
-}
zip : List a -> List b -> List (a, b)
zip = List.map2 Tuple.pair


{-| Create the list of all possible pairs from two other lists.

    cross [1, 2, 3] ['a', 'b'] ==
        [(1,'a'), (1,'b'), (2,'a'), (2,'b'), (3,'a'), (3,'b')]
-}
cross : List a -> List b -> List (a, b)
cross = join (\_ _ -> True)


{-| Create the list of all pairs that satisfy a condition.
-}
join : (a -> b -> Bool) -> List a -> List b -> List (a, b)
join p xs ys =
  xs |> List.concatMap (\x ->
  ys |> List.concatMap (\y ->
  if p x y then [(x, y)] else []))


{-| Select the list element following the value you provided, if one exists.

    inc ["do", "re", "mi"] "re" == Just "mi"
    inc ["do", "mi", "so"] "re" == Nothing
    inc ["do", "re", "mi"] "mi" == Nothing
    inc [] "mi" == Nothing
-}
inc : List a -> a -> Maybe a
inc enum elem =
  case enum of
    x1 :: x2 :: xs -> if x1 == elem then Just x2 else inc (x2 :: xs) elem
    _ -> Nothing


{-| Select the list element preceeding the value you provided, if one exists.

    dec ["do", "re", "mi"] "re" == Just "do"
    dec ["do", "mi", "so"] "re" == Nothing
    dec ["do", "re", "mi"] "do" == Nothing
    dec [] "do" == Nothing
-}
dec : List a -> a -> Maybe a
dec enum elem =
  case enum of
    x1 :: x2 :: xs -> if x2 == elem then Just x1 else dec (x2 :: xs) elem
    _ -> Nothing


{-| Find the first element of the list that satisfies the callback, or
an empty maybe if no list elements satisfy the callback.

    find isEven [3, 5, 4, 1, 2] == Just 4
    find isEven [3, 5, 5, 1, 7] == Nothing
    find isEven [] == Nothing
-}
find : (a -> Bool) -> List a -> Maybe a
find p xs = List.head (List.filter p xs)


{-| Provide a combining operation, a neutral element, and a mapping
function. Maps the mapping function over a list while reducing the
results using the combining function.
-}
mapReduce : (b -> b -> b) -> b -> (a -> b) -> List a -> b
mapReduce mult e eval =
  List.foldl (\x acc -> mult acc (eval x)) e


{-| Map a function that might fail over a list. The overall result fails
when the mapping function fails on at least one element.
-}
traverse : (a -> Maybe b) -> List a -> Maybe (List b)
traverse f =
  let
    reduce x acc =
      case f x of
        Nothing -> Nothing
        Just y -> Maybe.map (\ys -> y :: ys) acc
  in
  Maybe.map List.reverse << List.foldl reduce (Just [])


{-| Provide a key function to partition a list into sublists by key.
-}
groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy key =
  let
    f x = Dict.update (key x) (g x)
    g x y =
      case y of
        Nothing -> Just [x]
        Just xs -> Just (x :: xs)
  in
  Dict.map (\_ v -> List.reverse v) << List.foldl f Dict.empty
