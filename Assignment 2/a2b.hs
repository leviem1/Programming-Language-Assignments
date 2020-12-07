module A2b where

-- Removes all items from a list except the item provided
-- item - the item to exclude from removal
-- list - list to remove item from
-- Returns list of only item as an element
removeAllExcept :: (Eq a) => a -> [a] -> [a]
removeAllExcept item [] = []
removeAllExcept item (x:xs) = if item == x
  then removeAllExcept item xs ++ [x]
  else removeAllExcept item xs

-- Removes all elements of a list eqivalent to item
-- item - the item to remove
-- list - list to remove item from
-- Returns a list excluding item
removeAll :: (Eq a) => a -> [a] -> [a]
removeAll item [] = []
removeAll item (x:xs) = if item == x
  then removeAll item xs
  else removeAll item xs ++ [x]

-- Replaces all instances of item with repl within a list
-- item - element to replace
-- repl - element to replace item with
-- Returns list with all instances of item replaced with repl
substitute :: (Eq a) => a -> a -> [a] -> [a]
substitute item repl [] = []
substitute item repl (x:xs) = if item == x
  then substitute item repl xs ++ [repl]
  else substitute item repl xs ++ [x]

-- Merges three ordered list and preserves ordering
-- list1 - first list to merge
-- list2 - second list to merge
-- list3 - third list to merge
-- Returns a list of all elements in the original lists
-- with preserved ordering
mergeSorted3 :: (Ord a) => [a] -> [a] -> [a] -> [a]
mergeSorted3 [] [] [] = []
mergeSorted3 a b [] = merge2 a b
mergeSorted3 a [] b = merge2 a b
mergeSorted3 [] a b = merge2 a b
mergeSorted3 a b c = merge2 (merge2 a b) c 

-- Helper function that merges two lists and preserces ordering
-- list1 - first list to merge
-- list2 - second list to merge
-- Returns a list of all elements in the original lists
-- with preserved ordering
merge2 :: (Ord a) => [a] -> [a] -> [a]
merge2 [] [] = []
merge2 li [] = li
merge2 [] li = li
merge2 li1 li2 = if (head li1) <= (head li2)
                    then (head li1) : merge2 (tail li1) li2
                    else (head li2) : merge2 li1 (tail li2)

-- Data structure representing a tri-nary tree
data TriTree a = EmptyNode | TriNode a (TriTree a) (TriTree a) (TriTree a)

instance (Eq a) => Eq (TriTree a) where
  EmptyNode           == EmptyNode = True
  TriNode a la ma ra  == TriNode b lb mb rb = (a == b) &&
                                              (la == lb) &&
                                              (ma == mb) &&
                                              (ra == rb)
  _                   == _ = False

-- Retrieves the value of a node
-- node - TriNode to get value from
-- Returns value of node
nodeValue :: TriTree a -> a
nodeValue EmptyNode = error "Attempt to get field of empty TriNode!"
nodeValue (TriNode val lc mc rc) = val

-- Retrieves the left child of a node
-- node - TriNode to get child from
-- Returns left child node
leftChild :: TriTree a -> TriTree a
leftChild EmptyNode = error "Attempt to get field of empty TriNode!"
leftChild (TriNode val lc mc rc) = lc

-- Retrieves the middle child of a node
-- node - TriNode to get child from
-- Returns middle child node
middleChild :: TriTree a -> TriTree a
middleChild EmptyNode = error "Attempt to get field of empty TriNode!"
middleChild (TriNode val lc mc rc) = mc

-- Retrieves the right child of a node
-- node - TriNode to get child from
-- Returns right child node
rightChild :: TriTree a -> TriTree a
rightChild EmptyNode = error "Attempt to get field of empty TriNode!"
rightChild (TriNode val lc mc rc) = rc

-- Searches tree for item
-- item - item to search for
-- node - TriNode to begin search from
-- Returns true if item is found, false otherwise
inTree :: (Eq a) => a -> TriTree a -> Bool
inTree item EmptyNode = False
inTree item (TriNode val lc mc rc) = val == item || inTree item lc
                               || inTree item mc || inTree item rc

-- Creates a list of all leaf values
-- node - TriNode to begin search for leaves from
-- Returns a list of all leaf values
leafList :: TriTree a -> [a]
leafList EmptyNode = []
leafList (TriNode val EmptyNode EmptyNode EmptyNode) = [val]
leafList (TriNode val lc mc rc) = leafList lc ++ leafList mc ++ leafList rc

-- Applies a function to all nodes in tree
-- operation - operation to apply to each node
-- node - TriNode to start applying operation to recursively
-- Returns a TriTree containing the results of the operation
inOrderMap :: (a->b) -> TriTree a -> TriTree b
inOrderMap op EmptyNode = EmptyNode
inOrderMap op (TriNode a b c d) = TriNode (op a) (inOrderMap op b) (inOrderMap op c)
                                  (inOrderMap op d)

-- Applies a pre-order fold to all nodes in tree
-- operation - operation to apply to nodes
-- acc - accumulator value to use as base value
-- Returns the result of the fold of the operation
preOrderFold :: (b -> a -> b) -> b -> TriTree a -> b
preOrderFold op acc EmptyNode = acc
preOrderFold op acc (TriNode a b c d) = preOrderFold op (preOrderFold op (preOrderFold op (acc `op` a) b) c) d

