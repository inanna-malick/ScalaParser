package parser


abstract class Expr
case class Number(value: Double) extends Expr
case class StringLiteral(value: String) extends Expr
case class BooleanLiteral(value: Boolean) extends Expr
case class UnaryOp(operator: String, arg: Expr) extends Expr
case class BinaryOp(operator: String, left: Expr, right: Expr) extends Expr
case class Function(name: String, args: List[Expr]) extends Expr
case class ClipboardRef(name: List[ClipboardIdent]) extends Expr
case class LocalClipboardRef(name: List[ClipboardIdent]) extends Expr //'.' prefix
case class Parens(e: Expr) extends Expr

sealed trait ClipboardIdent{ val name: String}
case class StdClipboardIdent(name: String) extends ClipboardIdent
case class ClipboardCollection(name: String, key: Expr) extends ClipboardIdent