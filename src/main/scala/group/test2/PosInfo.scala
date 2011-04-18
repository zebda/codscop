package group.test2

import util.parsing.input.Position

/**
 * Created by IntelliJ IDEA.
 * User: anton
 * Date: 29.03.11
 * Time: 10:41
 * To change this template use File | Settings | File Templates.
 */

case class PosInfo[T](value:T, start: Position, end: Position) {
		override def toString = "[%s-%s]".format(start, end)
}