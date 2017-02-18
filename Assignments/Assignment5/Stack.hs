module Stack 
(	isEmpty
, 	empty
,	push
,	pop
,	top
,	size
) where


isEmpty :: Stack a -> Bool
empty :: Stack a
push :: Stack a -> a -> Stack a
pop :: Stack a -> (a,Stack a)
top :: Stack a -> a
size :: Stack a -> Int

type Stack a = [a] 

isEmpty stack = case length stack of 
	0 -> True
	_ -> False

empty = []

push stack element = [element] ++ stack

pop stack = (top stack , drop 1 stack)

top stack = stack!!0

size stack = length stack
