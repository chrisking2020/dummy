fun pow(b, e) = 
	let
		fun pow'(b, 0, acc) = acc
		  | pow'(b, e, acc) = pow'(b, e-1, acc*b)
	in
		pow'(b, e, 1)
	end;
  
fun evaluate(l) =
	let
		fun evaluate'([],s) = top(s)
		  | evaluate'(Bracket(_)::xs,s) = raise ParanthesesMissmatch
		  | evaluate'(Digit(n)::xs , s) = evaluate'(xs, push(s,Digit(n)))
		  | evaluate'(Operator(operation,prio)::xs,s) = 
		  		let
		  			val Digit(b)      = top(s)
		  			val Digit(a)      = top(pop(s))
		  			fun add(a,b)      = Digit(a+b)
		  			fun subtract(a,b) = Digit(a-b)
		  			fun multiply(a,b) = Digit(a*b)
		  			fun divide(a,b)   = Digit(a div b)
		  			fun power(a,b)    = Digit(pow(a,b))
		  			fun modolus(a,b)  = Digit(a mod b)
		  		in
		  			if operation = Plus  then evaluate'(xs, push(pop(pop(s)),add(a,b)))      else
		  			if operation = Minus then evaluate'(xs, push(pop(pop(s)),subtract(a,b))) else
		  			if operation = Times then evaluate'(xs, push(pop(pop(s)),multiply(a,b))) else
		  			if operation = Div 	 then evaluate'(xs, push(pop(pop(s)),divide(a,b)))   else
		  			if operation = Pow   then evaluate'(xs, push(pop(pop(s)),power(a,b)))    else
		  									  evaluate'(xs, push(pop(pop(s)),modolus(a,b)))
		  		end
	in
		evaluate'(l,EmptyStack)
	end;