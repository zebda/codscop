/**
 *
 */
package group.test2

import scala.io.Source
import scala.collection.mutable.{LinkedList, Stack}
import scala.collection.immutable.List

import org.apache.commons.lang.StringUtils
import java.io.File;
import java.net.URL;

/**
 * @author anton
 */
object App1 {

	val map: LinkedList[String] = new LinkedList[String]();
	//var lineIndex : Integer = 0;

	var block: Stack[String] = new Stack[String];
	var blockId = 0;

	var prevContent: String = "";
	var within: Boolean = false;

	def main(args: Array[String]): Unit = {

		val file = "/home/anton/Temp/AppCore.java"

		val res = this.getClass.getResource(".");

		findScripts(res.getFile)
				.map(url => url.getFile())
				.filter(path => path.endsWith(".java"))
				.filter(f =>
					!f.endsWith("Test120.java")    //uses 'enum' keyword as identifier
					&& !f.endsWith("Test127.java") //wrong constructor signature
				)
				.sortWith(_.compareTo(_) < 0)
				.foreach(measure2)
	}


	private def findScripts(path: String): List[URL] = {
		val fileWithScripts: File = new File(path);
		var locations: List[URL] = Nil;
		if (fileWithScripts.listFiles() == null)
			throw new IllegalArgumentException("Path " + path + " is not a directory.");

		for (file <- fileWithScripts.listFiles()) {
			if (file.isDirectory() && !file.getName().startsWith(".")) //to ignore ".svn" folder
				locations ::: findScripts(file.getPath());
			else
				locations = getURLOfFile(file) :: locations;
		}
		return locations;
	}

	private def getURLOfFile(file: File) =
		file.toURI().toURL();



	def measure2(path: String): List[String] = {
		var s:String = null;
		try {
			s = Source.fromFile(path, "UTF-8")
					.getLines()
					.mkString("\r\n");
		} catch {
			case e: Exception => System.err.println("-"+path); //e.printStackTrace()
		}
		if (s == null) return Nil;

		println(path)
		val result = new JavaParser().parse(s)
		if (!result.isDefined) {
			println("NONE")
			return Nil
		}
		val cons = result.get
		//println(cons)
		println(cons filter((x:String)=>
		x.startsWith("<any>"))) //	x.startsWith("<class>") || x.startsWith("<method>")))
		return cons
	}


	def measure(path: String): Unit = {
		try {
			Source.fromFile(path, "UTF-8")
					.getLines()
					.foreach(process);
			println(path)
		} catch {
			case e: Exception => System.err.println("-"+path); //e.printStackTrace()
		}
	}


	def process(s: String) = {
		var content: String = "";

		if (s.contains("*/"))
			within = false;



		if (StringUtils.isBlank(s))
			content = "<empty>";
		else if (startsWith(s, "//"))
			content = "<comment>";
		else if (startsWith(s, "/**")) {
			content = "<javadoc>";
			within = true;
		}
		else if (startsWith(s, "/*")) {
			content = "<multiline-comment>";
			within = true;
		}
		else if (endsWith(s, "*/"))
			content = prevContent
		else if (within)
			content = prevContent;
		else if (s.contains("{"))
			content = "<%s>".format(
				block.push("block id='%s'".format(incBlock)))
		else if (s.contains("}"))
			content = "</%s>".format(block.pop)
		else
			content = s;



		map + content
		//println(content)
		prevContent = content
	}

	def startsWith(input: String, prefix: String) =
		StringUtils.startsWith(StringUtils.stripStart(input, null), prefix);

	def endsWith(input: String, postfix: String) =
		StringUtils.endsWith(StringUtils.stripEnd(input, null), postfix);

	def incBlock() = {
		blockId = blockId + 1
		blockId
	}

}