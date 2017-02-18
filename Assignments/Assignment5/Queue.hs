module Queue
( isEmpty
 , 	enqueue
 , 	dequeue
 , 	peek 
 ,	size
 , 	empty)
 where 

 type Queue a = [a] 
 
 isEmpty :: Queue a -> Bool	
 enqueue :: Queue a -> a -> Queue a
 dequeue :: Queue a -> (a, Queue a)
 peek :: Queue a -> a
 size :: Queue a -> Int
 empty :: Queue a

 empty = []

 isEmpty queue = if (size queue == 0) then True else False

 enqueue queue element =  element : queue

 dequeue queue = (queue!!0, drop 1 queue)

 peek queue = queue!!0

 size queue = length queue