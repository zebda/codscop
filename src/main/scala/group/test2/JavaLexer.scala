package group.test2

import util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader.EofCh
import util.parsing.input.{Position, Positional}



/**
 * @author anton
 */
class JavaLexer extends StdLexical {

	override def token: Parser[Token] = {
		( comment1
		| string ^^ StringLit
		| identifier
		| number
		| EofCh  ^^^ EOF
		//| '\"' ~> failure("unclosed string literal")
		| delim
		| failure("illegal character")
		)
	}

	def comment1 =
		( pos(javadocComment) ^^ { p => Comment("javadoc") << p}
		| pos(multilineComment)  ^^ { p => Comment("multiline") << p}
		| pos(lineComment)  ^^ { p => Comment("singleline") << p}
		)

	def lineComment = '/' ~> '/' ~> rep(chrExcept(EofCh, '\n', '\r'))  ^^ { _ mkString "" }

	def multilineComment = '/' ~> '*' ~> rep(not('*' ~ '/') ~> chrExcept(EofCh)) <~ '*' <~ '/'  ^^ { _ mkString "" }

	def javadocComment = '/' ~> '*' ~> '*' ~> rep(not('*' ~ '/') ~> chrExcept(EofCh)) <~ '*' <~ '/'  ^^ { _ mkString "" }

	def identifier = identChar ~ rep( identChar | digit )   ^^ { case first ~ rest => processIdent(first :: rest mkString "") }

	def number = digit ~ rep( digit )  ^^ { case first ~ rest => NumericLit(first :: rest mkString "") }

	def string = (
			'\"' ~> rep(charSeq | chrExcept('\"', '\n', EofCh)) <~ '\"' ^^ { _ mkString "" }
			| '\'' ~> (charSeq | chrExcept('\'', '\n', EofCh)) <~ '\'' ^^ { _.toString }
			| '\'' ~> failure("unclosed string literal")
			| '\"' ~> failure("unclosed string literal")
	)

	def charSeq: Parser[String] =
		('\\' ~ '\"' ^^^ "\""
		|'\\' ~ '\\' ^^^ "\\"
		|'\\' ~ '/'  ^^^ "/"
		|'\\' ~ 'b'  ^^^ "\b"
		|'\\' ~ 'f'  ^^^ "\f"
		|'\\' ~ 'n'  ^^^ "\n"
		|'\\' ~ 'r'  ^^^ "\r"
		|'\\' ~ 't'  ^^^ "\t"
		|'\\' ~> 'u' ~> unicodeBlock)


	private def unicodeBlock = hexDigit ~ hexDigit ~ hexDigit ~ hexDigit ^^ {
		case a ~ b ~ c ~ d =>
			new String(Array(Integer.parseInt(List(a, b, c, d) mkString "", 16)), 0, 1)
	}

	val hexDigits = Set[Char]() ++ "0123456789abcdefABCDEF".toArray
	def hexDigit = elem("hex digit", hexDigits.contains(_))

	override def whitespace: Parser[Any] = {
		//println("before wht")
		val wht = whitespace1;
		//println(wht)
		wht
	}


	var lineNumber = 0
	def whitespace1: Parser[Any] = rep(
		whitespaceChar
    )


	//def pos[T](p: Parser[T]): Parser[PosToken[T]] =
	//	positioned(p ^^ {v => new PosToken[T](v)})

	def pos[T](p: Parser[T]): Parser[PosInfo[T]] =
		Parser { in =>
			p(in) match {
			  case Success(v,in1) => Success(PosInfo(v, in.pos, in1.pos), in1)
			  case ns: NoSuccess => ns
			}
		}



	/** The class of comment tokens */
	case class Comment(kind:String) extends PosToken[String] {
		override def toString = position+"<"+kind+">" //"/*"+kind+chars+"*/"
	}

	case class PosToken[T]() extends Token { // with Positional

		private var opos: Option[PosInfo[T]] = None
		private var oval: Option[T] = None

		def this(value: T) {
			this()
			this.oval = Some(value)
		}

		def this(position: PosInfo[T]){
			this(position.value)
			opos = Some(position)
		}

		def << (position: PosInfo[T]): PosToken[T] = {
			this.opos = Some(position)
			this.oval = Some(position.value)
			this
		}

		def value = oval
		def position = opos
		override def chars = value.toString
		override def toString = "%s %s".format(opos, oval)
	}



}