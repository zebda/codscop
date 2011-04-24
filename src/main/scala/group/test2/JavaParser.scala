package group.test2

import util.parsing.combinator.syntactical.StandardTokenParsers
import util.parsing.combinator.ImplicitConversions
import util.parsing.combinator.token.{StdTokens, Tokens}



/**
 * @author anton
 */
class JavaParser extends StandardTokenParsers with ImplicitConversions {
	override type Tokens = StdTokens
	override val lexical = new JavaLexer
	import lexical.{Comment}

	val modifierKeys = Array(
			"public", "protected", "private", "final", "static", "abstract", "synchronized", "native", "transient", "volatile")

	lexical.reserved += (
			"package", "import",
			"class", "interface", "enum", "extends", "implements", "super",
			//"public", "protected", "private", "final", "static", "abstract", "synchronized", "native", "transient", "volatile",
			"for", "break", "continue", "if", "else", "switch", "case",
			"return", "new", "throw"
		)
	lexical.reserved ++= modifierKeys

	lexical.delimiters += (".", ",", ";", "@", ":", "{", "}", "(", ")", "<", ">", "[", "]", "*", "?", "=")
							//"+", "-", "/", "*", "%", "&&", "||")

	def parse(s : String) : Option[List[String]] = {
	//	println("lexing")
		var tokens = new lexical.Scanner(s)
	//	println("lexed")
	//	while (!tokens.rest.atEnd) {
	//		println(tokens.first)
	//		tokens = tokens.rest
	//	}
		//return None
		phrase(compilationUnit)(tokens) match {
      		case Success(result, _) => Some(result)
      		case _ => None
    	}
	}


	def compilationUnit : Parser[List[String]] = (
		rep1(
			classDeclaration                ^^ (_.toString)
			| packageDeclaration            ^^ (_.toString)
			| importDeclaration				^^ (_.toString)
			| comment                       ^^ (_.toString)
			| trace
			| unknown 		 				^^ { (s:String)=> /*println(s);*/s}
		) ^^ { (statements : List[String]) => statements}
	)


	def trace: Parser[String] = (
			methodDeclaration               ^^ (_.toString)
			| constructorDeclaration        ^^ (_.toString)
			| staticConstructorDeclaration  ^^ (_.toString)
			| fieldDeclaration              ^^ (_.toString)
			| markerAnnotation              ^^ (_.toString)
	)


	def classDeclaration: Parser[ClassStructure] = (
			modifiers ~ "class" ~ ident
			~ rep(anyBut("{", a=>"<any>"))
			~ classBody
	)	^^ {case ms ~ _ ~ id ~ _ ~ cb  => ClassStructure(ms, id.toString, cb)} //"<class> " + id.toString + cb.toString }


	def classBody: Parser[List[CodeStructure]] =  (
		"{"
		~> rep(
			classDeclaration
			| methodDeclaration
			| constructorDeclaration
			| staticConstructorDeclaration
			| fieldDeclaration
			| comment
			| block ^^^ CodeStructure("in-class block")
			| ";" ^^^ CodeStructure("garbage")
			//| anyBut ("}", a=>"<any>")
			//| unknown ^^^ "" 				//^^ { (s:String)=> println(s);s}
		)
		<~ "}"
	)

	def packageDeclaration =
		"package" ~> name <~ ";"	^^ (NamedCodeStructure("package", _))

	def importDeclaration =
		"import" ~> opt("static") ~> name <~ opt("." ~ "*") <~ ";"	^^ (NamedCodeStructure("import", _))

	def methodDeclaration = (
			modifiers ~ opt(typeParams) ~ typeRef ~ ident <~ formalParameters
			<~ rep(anyBut("{" | ";", a=>"<any>"))
			<~ (block | ";")
	)	^^ {case ms ~ tps ~ t ~ id => new MethodStructure(ms, id.toString)}

	def constructorDeclaration =
		modifiers ~ ident <~ formalParameters <~ block  ^^ {case ms ~ id => MethodStructure(ms, id.toString, true)}

	def staticConstructorDeclaration =
		"static" ~ block		^^^ MethodStructure("static" :: Nil, "", true)

	def fieldDeclaration = (
			modifiers ~ typeRef
			~ variableDeclaration
			<~ opt("=" ~ rep(anyBut(";", a=>"<any>")))
			<~ ";"
	)	^^ {case ms ~ t ~ id => new FieldStructure(ms, id.toString)}

	def variableDeclaration =
		ident ~ rep("," ~> ident)

	def block: Parser[List[String]] =	"{" ~> rep(statement ^^ { _.toString }) <~ "}" // ^^ { _.toString }

	def statement = (
		block
		|rep1(anyBut("{" | "}", a=>"<statement>"))
	)

	def formalParameters = "(" ~> rep(anyBut(")", a=>"<anyparam>")) <~ ")"

	//def formalParameter = modifiers ~ typeRef ~ opt("...") ~ ident

	def typeRef: Parser[String] = name <~ opt("[" ~ "]")

	def modifiers = rep(modifier | annotation)

	def modifier = (
		"public" | "protected" | "private" | "final" | "static" | "abstract" | "synchronized" | "native" | "transient" | "volatile"
	) //^^ { (m:String) => m}

	def annotation = markerAnnotation

	def markerAnnotation = "@" ~> name <~ opt(formalParameters)   ^^ (attr => "@" + attr)

	def name =
		ident ~ opt(typeParams) ~ rep("." ~> ident ~ opt(typeParams)) ^^ {case first ~ rest => (first :: rest) mkString "."}

	def typeParams =
		"<" ~> typeParam ~ rep("," ~ typeParam) <~ ">"  ^^ {case first ~ rest => (first :: rest) mkString "."}

	def typeParam =
		opt(("?" | ident) ~ ("extends" | "super")) ~> typeRef

	def unknown = elem("any", !_.equals(lexical.EOF)) ^^ (a => "<any>" + a.toString)

	def eof = elem("eof", _.equals(lexical.EOF)) ^^ (a => "<eof>" + a.toString)

	/** A parser which matches a comment */
	def comment =
		elem("comment", _.isInstanceOf[Comment]) ^^ {e =>
			val c = e.asInstanceOf[Comment]
			CodeStructure(c.kind) << c.position
		}

	def anyBut[T](p: => Parser[T], t: Elem => T): Parser[T] =
		not(p) ~ not(eof) ~>  {
			type ElType = this.Elem
			new Parser[T] {
				type Elem = ElType
				def apply(in: Input) =
					new Success[T](t(in.first), in.rest)
			}
		}
}