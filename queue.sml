exception EmptyQueue;

abstype 'a queue = Queue of 'a list
with 
	val emptyQueue = Queue [];
  
	fun dequeue(Queue([])) = raise EmptyQueue
		|dequeue(Queue(x :: xs)) = Queue(xs);
  
  	fun enqueue(Queue(xs), x)  = Queue(xs@[x]);
  
  	fun first(Queue([])) = raise EmptyQueue
    	|first(Queue (x :: xs)) = x;

    fun hasNextQ(Queue([])) = false
    	|hasNextQ(Queue(x::xs)) = true;

    fun queueToList(Queue([])) = []
      | queueToList(Queue(x::xs)) = x::xs;
end;