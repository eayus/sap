module Sap.Util

import Data.List.Quantifiers


%default total


public export
Error : Type -> Type
Error = Either String


public export
allTautology : ((x : a) -> p x) -> {xs : List a} -> All p xs
allTautology f {xs = []}      = []
allTautology f {xs = x :: xs} = f x :: allTautology f


-- "HashMap"

public export
Map : Type -> Type -> Type
Map a b = List (a, b)
