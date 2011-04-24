package group.test2

/**
 * @author anton
 */
case class CodeStructure(kind: String) {//, subStructures: List[CodeStructure]) {


	//def this(kind: String) = this(kind, Nil)


	protected var opos: Option[PosInfo[String]] = None

	def position = opos

	def << (position: PosInfo[String]): CodeStructure = {
		this.opos = Some(position)
		this
	}

	def << (position: Option[PosInfo[String]]): CodeStructure = {
		this.opos = position
		this
	}

	//override def toString = "%s <%s> {%s}".format(opos, kind, subStructures mkString ", ")

	override def toString = "%s <%s>".format(opos, kind)

}



trait Modifiers {
	val modifiers: List[String]
}

trait Substructures {
	val substructures: List[CodeStructure]
}




case class NamedCodeStructure(override val kind: String, name: String) extends CodeStructure(kind)  {
	override def toString = "%s %s".format(super.toString, name)
}

case class MethodStructure(modifiers: List[String], override val name: String, isConstructor: Boolean)
		extends NamedCodeStructure("method", name) with Modifiers  {

	def this(modifiers: List[String], name: String) = this(modifiers, name, false)

	override def toString = "%s %s: %s".format(
		super.toString, if (isConstructor) "constructor" else "", modifiers mkString ", ")
}


case class FieldStructure(modifiers: List[String], override val name: String)
		extends NamedCodeStructure("field", name) with Modifiers  {

	override def toString = "%s: %s".format(
		super.toString, modifiers mkString ", ")
}


case class ClassStructure(modifiers: List[String], subkind: String,
						  override val name: String, substructures: List[CodeStructure])
		extends NamedCodeStructure("class", name) with Modifiers with Substructures {

	override def toString = "%s: %s %s {%s}".format(
		super.toString, subkind, modifiers mkString ", ", "\n\t" + (substructures mkString "\n\t"))
}