module LinkedList 
(	insert
,	remove
,	size
, 	isEmpty
, 	empty
) where

isEmpty :: LinkedList a -> Bool
insert :: LinkedList a -> a -> Int -> LinkedList a
remove :: LinkedList a -> Int -> (a, LinkedList a)
size :: LinkedList a -> Int
empty :: LinkedList a

type LinkedList a = [a]

empty = []

insert list element index = take index list ++ [element] ++ drop index list

remove list index = (list!!index , take index list++ drop (index+1) list)

size list  = length list

isEmpty list = if (size list == 0 ) then True else False
