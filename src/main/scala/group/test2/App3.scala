package group.test2

/**
 * Created by IntelliJ IDEA.
 * User: anton
 * Date: 19.03.11
 * Time: 2:40
 * To change this template use File | Settings | File Templates.
 */


import scala.io.Source
import org.apache.commons.lang.StringUtils

object App3  {


	def main(args: Array[String]): Unit = {

		var file = "/home/anton/Temp/AppCore.java"
		file = "/home/anton/Temp/TestIdeaProject/src/main/resources/group/test2/Test42.java"

		val s = Source.fromFile(file, "UTF-8")
				.getLines()
				.mkString("\r\n");
		//println(s)

		val s1 = "class AppCore"

		val parser = new JavaParser
		val result = parser.parse(s)
		val cons = result.get
		//println(cons)

		cons
			.filter((x:String)=>
				StringUtils.isNotBlank(x))
			.foreach((x:String)=>
				println(x))
	}

}