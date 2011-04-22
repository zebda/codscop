package group.test2

/**
 * @author anton
 */
object App2 {

	def main(args: Array[String]): Unit = {

		val s = "\"gsdfg\"\r\n\"ehusdk\" {\"hkjd\" = \"!!!!\";}\r\n"
		val s2 = """



    "glossary": {
        "title": "example glossary",
		"GlossDiv": {
            "title": "S",
			"GlossList": {
                "GlossEntry": {
                    "ID": "SGML",
					"SortAs": "SGML",
					"GlossTerm": "Standard Generalized Markup Language",
					"Acronym": "SGML",
					"Abbrev": "ISO 8879:1986",
					"GlossDef": {
                        "para": "A meta-markup language, used to create markup languages such as DocBook.",
						"GlossSeeAlso": ["GML", "XML"]
                    },
					"GlossSee": "markup"
                }
            }
        }
    }"""
		//ExprParser.parse("Hello world!!")

		//type Tokens = StdTokens
		//val lexical = new StdLexical
		val lexical = new Lexer
		lexical.reserved ++= List("true", "false", "null")
		lexical.delimiters ++= List("{", "}", "[", "]", ":", ",")
		//println(lexical.whitespace(new Scanner("\n\n\n\n")))

		val tokens = new lexical.Scanner(s2)
		println(tokens.first)
		println(tokens.rest.first)
		println(tokens.rest.rest.first)
		println(tokens.rest.rest.rest.first)
		println(tokens.rest.rest.rest.rest.first)
		println(tokens.rest.rest.rest.rest.rest.first)
		println(tokens.rest.rest.rest.rest.rest.rest.first)
	}

}