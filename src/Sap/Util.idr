module Sap.Util

import Data.List
import Data.List.Quantifiers


%default total


public export
allTautology : ((x : a) -> p x) -> {xs : List a} -> All p xs
allTautology f {xs = []}      = []
allTautology f {xs = x :: xs} = f x :: allTautology f


public export
indexAll : {0 xs : List a}
        -> (n : Nat)
        -> {auto 0 ib : InBounds n xs}
        -> All p xs
        -> p (index n xs)
indexAll Z     (x :: xs) {ib = InFirst}   = x
indexAll (S k) (x :: xs) {ib = InLater _} = indexAll k xs


public export
getAny : {xs : List a} -> Any p xs -> a
getAny {xs = (x :: xs)} (Here p)  = x
getAny {xs = (x :: xs)} (There p) = getAny p


public export
allByProp : (0 prop : a -> Type) -> All p xs -> {auto elem : Any prop xs} -> p (getAny elem)
allByProp prop (x :: xs) {elem = (Here eq)} = x
allByProp prop (x :: xs) {elem = (There p)} = allByProp prop xs


-- "HashMap"

public export
Map : Type -> Type -> Type
Map a b = List (a, b)
