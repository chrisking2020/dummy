

exception ParanthesesMissmatch;

datatype parantheses = Left | Right;
datatype operation = Plus | Minus | Times | Div | Pow | Mod;
datatype token = Operator of (operation * int) | Digit of int | Bracket of parantheses | Expression of token list;

use "queue";
use "stack";
use "evaluate";
(* 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3 *)
val exp2 = Expression([Digit(7), Operator(Times,2), Digit(3), Operator(Minus,1),Digit(16)])
val exp = [Digit(9), Operator(Plus,1), Digit(24), Operator(Div,2), Bracket(Left), Digit(7), Operator(Minus,1), 
			Digit(3), Bracket(Right), Operator(Minus,1), exp2];



exception EmptyStackException;
(*
fun toPost(l)
	let
		val stack_empty = isEmpty(stack);
		val list_empty = isEmpty(list);
	in
		if stack_empty andalso list_empty then
			return done
		else if list_empty then
			pop_stack()
		else
			case list of Digit(n)::rest 	 => 
						 Bracket(Left)::rest =>
	*)		



fun toPostFix(l) =
	let
		fun	toPostFix'([], EmptyStack, q) = q (*Base case*)
		  | toPostFix'([], s, q) = toPostFix'([], pop(s), enqueue(q,top(s))) (*Input list is empty, but there are still items in the operator stack*)
		  | toPostFix'(Digit(n)::xs, s, q) = toPostFix'(xs,s,enqueue(q,Digit(n))) (*Input is a digit, moves it to the output queue*)
		  | toPostFix'(Bracket(Left)::xs, s, q) = toPostFix'(xs, push(s,Bracket(Left)),q) (*Input is a left parantheses, adds it to the operator stack*)
		  
		  (*Input is right parantheses, move operators from stack to queue untill the matching left parantheses is found*)
		  | toPostFix'(Bracket(Right)::xs, s, q) = if (top(s) = Bracket(Left)) then toPostFix'(xs,pop(s),q) else toPostFix'(Bracket(Right)::xs, pop(s), enqueue(q,top(s)))
		  | toPostFix'(Bracket(Right)::xs, EmptyStack, q) = raise ParanthesesMissmatch(*Left parantheses not found, raise exception*)
		  
		  (*Input is a new expression. Turns expression to postfix, evaluates it and puts it in the output queue*)
		  | toPostFix'(Expression(x::xs)::ys, s, q) = toPostFix'(ys, s, enqueue(q,evaluate(toPostFix(x::xs))))

		  | toPostFix'(Operator(opr, prio)::xs, EmptyStack, q) = toPostFix'(xs, push(EmptyStack, Operator(opr, prio)), q)(*Operator added to otherwhise empty stack*)
		  | toPostFix'(Operator(opr, prio)::xs, s, q) = (*If operator on stack have higher priority than input, move from stack to queue, else add input to stack*)
				let
					val Operator(opr', prio') = if top(s) <> Bracket(Left) then top(s) else Operator(Plus,0)
				in
					if (prio' > prio) then
						toPostFix'( Operator(opr, prio)::xs, pop(s), enqueue(q,top(s)))
					else
						toPostFix'(xs, push(s, Operator(opr, prio)), q)
				end
	in
		queueToList(toPostFix'(l,EmptyStack,emptyQueue))
	end;

use "io";
	(*evaluate(queueToList(toPostFix'(formatInput(explode(str)),emptyStack,emptyQueue)),emptyStack);*)










