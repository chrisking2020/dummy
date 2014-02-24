fun extractWords(str) =
let
 	fun words(index, str, wordList) = 
 		if index >= size str then wordList else
		if String.sub(str,index) = #" " then 
	 		words(0, String.extract(str,index+1,NONE), String.substring(str,0,index)::wordList)
    	else
		 	words(index+1, str, wordList)

 in
 	List.rev(words(0,str^" ",[]))
 end ;

fun opToString(Operator(Plus,_)) =  "+"
  | opToString(Operator(Minus,_)) = "-"
  | opToString(Operator(Times,_)) = "*"
  | opToString(Operator(Div,_)) =   "/"
  | opToString(Operator(Pow,_)) =   "^"
  | opToString(Operator(Mod,_)) =   "mod"
  | opToString(Digit(n)) = Int.toString(n)
  | opToString(Bracket(Left)) = "("
  | opToString(Bracket(Right)) = ")";


fun formatInput(nil) = []
  | formatInput(x::xs) = 
	let
		fun formatInput'(#"0") = Digit(0) 
		  |	formatInput'(#"1") = Digit(1)
		  | formatInput'(#"2") = Digit(2)
		  | formatInput'(#"3") = Digit(3)
		  | formatInput'(#"4") = Digit(4)
		  | formatInput'(#"5") = Digit(5)
		  | formatInput'(#"6") = Digit(6)
		  | formatInput'(#"7") = Digit(7)
		  | formatInput'(#"8") = Digit(8)
		  | formatInput'(#"9") = Digit(9)
		  | formatInput'(#"+") = Operator(Plus,1)
		  | formatInput'(#"-") = Operator(Minus,1)
		  | formatInput'(#"*") = Operator(Times,2)
		  | formatInput'(#"/") = Operator(Div,2)
		  | formatInput'(#"%") = Operator(Mod,2)
		  | formatInput'(#"^") = Operator(Pow,3)
		  | formatInput'(#"(") = Bracket(Left)
		  | formatInput'(#")") = Bracket(Right)
	in
		formatInput'(x)::formatInput(xs)
	end;

fun formatOutput(queue) = 
	let
		fun formatOutput'(Digit(n)) = Int.toString(n)^" "
		   |formatOutput'(Operator(Plus,_))  = "+ "
		   |formatOutput'(Operator(Minus,_)) = "- "
		   |formatOutput'(Operator(Times,_)) = "* "
		   |formatOutput'(Operator(Div,_))   = "/ "
		   |formatOutput'(Operator(Pow,_))   = "^ "
		   |formatOutput'(Operator(Mod,_))   = "mod "
	in
		if (hasNextQ(queue)) then
			formatOutput'(first(queue))^formatOutput(dequeue(queue))
		else
			""
	end;

fun f(str) = 
	let
		val deleteWhiteSpace = List.filter (fn e => ord(e) <> 32)
		val input = formatInput(deleteWhiteSpace (explode(str)))
		val rpn = toPostFix(input)
		
		val eval1337 = evaluate(rpn)
	in
		eval1337
	end;
(*fun words(index, str, wordList) = if index >= size str then wordList else
								  if String.sub(str,index) = #" " then 
								 		words(index+1, str,index::wordList)
								  else
								 	words(index+1, str, wordList)*)