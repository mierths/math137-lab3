module Lib.Maybe exposing (..)

{-| Extra functions to make working with maybes easier.

# Create
@docs assert

# Access
@docs reduce

# Combine
@docs compose, paired, orElse

# Utilities
@docs filter

-}


{-| Keep a pure value if a condition is met.

    x |> assert True  == Just x
    x |> assert False == Nothing
-}
assert : Bool -> a -> Maybe a
assert p x = if p then Just x else Nothing


{-| Consume a maybe value by providing a callback to run if the value is
present and a fallback value in case it is not. Conceptually similar to
pattern matching, but in the form of a function.

    Nothing |> reduce "nope" String.fromInt = "nope"
    Just 3 |> reduce "nope" String.fromInt = "3"
-}
reduce : b -> (a -> b) -> Maybe a -> b
reduce y f xs = case xs of
    Nothing -> y
    Just x -> f x


{-| Chain together two functions that return maybes.

    compose f g Nothing  == Nothing
    compose f g (Just x) == Nothing if g x == Nothing
    compose f g (Just x) == f y     if g x == Just y
-}
compose : (b -> Maybe c) -> (a -> Maybe b) -> a -> Maybe c
compose f g a = g a |> Maybe.andThen f


{-| Create a pair from two maybes, failing if either is `Nothing`.

    paired (Just x) Nothing  == Nothing
    paired Nothing  (Just y) == Nothing
    paired (Just x) (Just y) == Just (x, y)
-}
paired : Maybe a -> Maybe b -> Maybe (a, b)
paired = Maybe.map2 Tuple.pair


{-| Replace a maybe with a fallback in case it is empty.

    Just x  |> orElse fallback == Just x
    Nothing |> orElse fallback == fallback
-}
orElse : Maybe a -> Maybe a -> Maybe a
orElse fallback primary =
  case primary of
    Just _ -> primary
    Nothing -> fallback


{-| Keep a maybe value if it satisfies the callback.

    filter p Nothing  == Nothing
    filter p (Just a) == Nothing if p a == False
    filter p (Just a) == Just a  if p a == True
-}
filter : (a -> Bool) -> Maybe a -> Maybe a
filter p m = m |> Maybe.andThen (\a -> if p a then m else Nothing)
