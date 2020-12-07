import Data.List
import Data.Char

onlyLowercase::[String]->[String]
onlyLowercase [] = []
onlyLowercase (x:xs) = if isLower (head x)
                          then x : onlyLowercase xs
                          else onlyLowercase xs

longestString::[String]->String
longestString [] = ""
longestString [a] = a
longestString l = foldl (\x -> ) l

longestStringHelper::(Int->Int->Bool)->[String]->String
longestStringHelper f [] = ""
longestStringHelper f [a] = a
longestStringHelper f (a:b:cs) = if (f (length a) (length b))
                                 then longestStringHelper f (a:cs)
                                 else longestStringHelper f (b:cs)
  
allAnswers::(a->Maybe[b])->[a]->Maybe[b]
allAnswers f [] = error "Calling allAnswers on empty list"
allAnswers f [a] = f a
allAnswers f (a:as) =
  let
    helper acc [] = Just acc
    helper acc (x:xs) =
      case (f a) of
        Nothing -> Nothing
        Just list -> helper (acc ++ list) as
  in
    allAnswers f as
