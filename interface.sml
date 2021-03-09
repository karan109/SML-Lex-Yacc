val error = ref false
val error_list = ref ""
fun head([]) = ""
    | head((x:string)::ls) = x
structure CalcLrVals = CalcLrValsFun(structure Token = LrParser.Token)
structure CalcLex = CalcLexFun(structure Tokens = CalcLrVals.Tokens);
structure CalcParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = CalcLrVals.ParserData
     	       structure Lex = CalcLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,col:int) =
		    	(print("Syntax Error:" ^ (Int.toString pos) ^ ":" ^ (Int.toString col) ^ ":" ^ s ^ "\n\n");
                error_list := ("Syntax Error:" ^ (Int.toString pos) ^ ":" ^ (Int.toString col) ^ ":" ^ s ^ "\n\n")^(!error_list))
		in
		    (error := true; CalcParser.parse(0,lexstream,print_error,()))
		end


fun stringToLexer str =
    let val done = ref false
    	val lexer=  CalcParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end

fun parse (lexer) =
    let val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = CalcParser.Stream.get lexer
    in
        if CalcParser.sameToken(nextToken, dummyEOF) then 
            if (!error) = false then result 
            else (print(!error_list); result)
 	else (print("Warning: Unconsumed input \n"); result)
    handle CalcParser.ParseError => (print("Warning: Unconsumed input \n"); result)
    end

val file_name = TextIO.openIn (head(CommandLine.arguments()));
val input_str = TextIO.inputAll file_name;
(parse o stringToLexer) (input_str);