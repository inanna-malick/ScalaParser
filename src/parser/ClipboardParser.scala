package parser

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import org.scalatest.Assertions._

abstract class Expr
//case class Variable(name: String) extends Expr //all refs to raw identifiers are interpreted as clipboard access
case class Number(value: Double) extends Expr
case class StringLiteral(value: String) extends Expr
case class UnaryOp(operator: String, arg: Expr) extends Expr
case class BinaryOp(operator: String, left: Expr, right: Expr) extends Expr
case class Function(name: String, args: List[Expr]) extends Expr
abstract class ClipboardIdent
case class StdClipboardIdent(name: String) extends ClipboardIdent
case class ClipboardCollection(name: String, key: Expr) extends ClipboardIdent
case class ClipboardRef(name: List[ClipboardIdent]) extends Expr //something in the clipboard
//something in the clipboard, preceding dot (local reference)
case class LocalClipboardRef(name: List[ClipboardIdent]) extends Expr 


object JavaOut {
	
	def buildExpr(e: Expr): String = e match { 
		case Number(n) => n.toString
		case StringLiteral(s) => s""""$s"""""
		case BinaryOp(op, lhs, rhs) => buildExpr(lhs) + op + buildExpr(rhs)
		case Function(f, args) => f + "(" + args.map{buildExpr}.mkString(",") + ")"
		case ClipboardRef(refs) => refs.map{buildRef}.mkString(".")
		case LocalClipboardRef(refs) => refs.map{buildRef}.mkString(".") //whatever, let's just simplify this. local is always in frame
	}
	
	def buildRef(c: ClipboardIdent): String = c match {
		case StdClipboardIdent(n) => s"""resolve("$n")"""
		case ClipboardCollection(n, expr) => val e = buildExpr(expr); s"""resolve("$n", $e)"""
	}

}



/*
object ScalaOut {
	type Result = Map[String, Any] => Double
	
	def buildExpr(e: Expr): Result = e match { 
		case Number(n) => env => println(n); n.toDouble
		//case StringLiteral(s) => env => s
		case BinaryOp(op, lhs, rhs) => env => println((op, lhs, rhs)); buildOp(op)(buildExpr(lhs)(env), buildExpr(rhs)(env))
		case Function(f, args) => env => buildFunction(f)(0.0) //???what the fuck, whatever, all functions are just println
		case ClipboardRef(refs) => env => println(refs.map{buildRef}.mkString(".")); 0.0
		case LocalClipboardRef(refs) => env => println(refs.map{buildRef}.mkString(".")); 0.0 //whatever, let's just simplify this. local is always in frame
	}
	
	def buildFunction(s: String) = (x: Double) => {println(s"!!: $x"); x}
	
	def buildOp(s: String): (Double, Double) => Double = s match {
		case "+" => (a, b) => a + b
		case "-" => (a, b) => a - b
	}
	
	def buildRef(c: ClipboardIdent): String = c match {
		case StdClipboardIdent(n) => s"""resolve("$n")"""
		case ClipboardCollection(n, expr) => val e = buildExpr(expr); s"""resolve("$n", $e)"""
	}

}
object ScalaBuilder extends ClipBoardParser{
	def apply(s: String): String = parseAll(expr, s) match {
			case Success(t, _) =>  val r = ScalaOut.buildExpr(t); println(r); r(Map()).toString
			case NoSuccess(msg, _) => throw new IllegalArgumentException(
				"Could not parse '" + s + "': " + msg)
		} 
}*/

object ExprBuilder extends ClipBoardParser{
	def apply(s: String): String = parseAll(expr, s) match {
			case Success(t, _) =>  JavaOut.buildExpr(t)
			case NoSuccess(msg, _) => throw new IllegalArgumentException(
				"Could not parse '" + s + "': " + msg)
		} 
}


//note: ident allows for java keywords, only checks for [java-letter][java-letter/number]*
class ClipBoardParser extends JavaTokenParsers {
	
	def clipboardIdent: Parser[ClipboardIdent] =
		(ident~"("~expr~")") ^^ {case c~"("~e~")" => ClipboardCollection(c, e)} |
		ident ^^ {StdClipboardIdent}

	def clipboardRef: Parser[Expr] =
		rep1sep(clipboardIdent, ".") ^^ { refs => ClipboardRef(refs) } |
		rep1("."~>clipboardIdent) ^^ { refs => LocalClipboardRef(refs) }
	
	//operators
	def op: Parser[String] = "+" | "-" | "*" | "/"
	
	def binaryOp: Parser[Expr] = 
		(expr~op~expr) ^^ { case lhs~op~rhs => BinaryOp(op, lhs, rhs) }
	
	def expr: Parser[Expr] =
		binaryOp |
		factor | 
		func | 
		clipboardRef

	def func: Parser[Expr] =  
		("@"~ident~"("~repsep(expr, ",")~")") ^^ {
			case "@"~f~"("~Nil~")" => Function(f, List())
			case "@"~f~"("~args~")" => Function(f, args)
		}

	def factor: Parser[Expr] =
		"("~>expr<~")" | // <- should this be elsewhere?
		floatingPointNumber ^^ { x => Number(x.toFloat) } |
		stringLiteral ^^ { s => StringLiteral(s.drop(1).dropRight(1))}
}

class ClipBoardParsersTest extends ClipBoardParser with FlatSpec with ShouldMatchers {

	private def assertFail[T](input: String)(implicit p: Parser[T]) {
		evaluating(parsing(input)) should produce[IllegalArgumentException]
	}

	private def parsing[T](s: String)(implicit p: Parser[T]): T = {
		parseAll(p, s) match {
			case Success(t, _) =>  t
			case NoSuccess(msg, _) => throw new IllegalArgumentException(
				"Could not parse '" + s + "': " + msg)
		}
	}
	
	
	"The ExpressionParsers" should "parse simple expressions" in {
		//just declare the parser to test once and mark it implicit
		implicit val parserToTest = expr
		parsing("15") should equal(Number(15))
		parsing("5 + 5") should equal(BinaryOp("+", Number(5), Number(5)))
		parsing("5 + 5 + 5") should equal(BinaryOp("+", Number(5), BinaryOp("+", Number(5), Number(5))))		
		//println(parsing("5 + 5 * 5"))
		assertFail("5 +")
		parsing("(5 + 5) + 5") should equal(BinaryOp("+", BinaryOp("+", Number(5), Number(5)), Number(5)))
		assertFail("5 + (5 + 5")
		parsing("(5 + 5) + (5 + 5)") should equal(BinaryOp("+", BinaryOp("+", Number(5), Number(5)), BinaryOp("+", Number(5), Number(5))))
		parsing("(5 + 5) + (5 - 5)") should equal(BinaryOp("+", BinaryOp("+", Number(5), Number(5)), BinaryOp("-", Number(5), Number(5))))
		parsing("\"foobar\"") should equal(StringLiteral("foobar"))
		parsing("\"foobar\" + 5") should equal(BinaryOp("+", StringLiteral("foobar"), Number(5)))
		assertFail("\"foo")
	}
	
	
	"The ExpressionParsers" should "parse function calls" in {
		//just declare the parser to test once and mark it implicit
		implicit val parserToTest = expr
		parsing("@doThings(5 + 5, 5)") should equal(Function("doThings", List(BinaryOp("+", Number(5), Number(5)), Number(5))))
		assertFail("@doThings(5 + 5 5)")
		assertFail("@doThings(5 + 5, )")
		parsing("@doSideEffect()") should equal(Function("doSideEffect", List()))
		assertFail("@doSideEffects")
		parsing("foo.bar.baz") should equal(ClipboardRef(List("foo", "bar", "baz").map{StdClipboardIdent}))
		parsing(".bar.baz") should equal(LocalClipboardRef(List("bar", "baz").map{StdClipboardIdent}))
		assertFail("foo.bar.")
		parsing("@doThings(5 + 5, 5, foo.bar.baz)") should equal(Function("doThings", 
				List(BinaryOp("+", Number(5), Number(5)), Number(5), ClipboardRef(List("foo", "bar", "baz").map{StdClipboardIdent}))))
		parsing("@doThings(5 + 5, 5, .foo.bar.baz)") should equal(Function("doThings", 
				List(BinaryOp("+", Number(5), Number(5)), Number(5), LocalClipboardRef(List("foo", "bar", "baz").map{StdClipboardIdent}))))
		parsing("foo(5).bar") should equal(ClipboardRef(List(ClipboardCollection("foo", Number(5)), StdClipboardIdent("bar"))))
		assertFail("foo(5")
		assertFail("foo 5")
		parsing("foo(5).bar(5 + 3)") should equal(ClipboardRef(List(
				ClipboardCollection("foo", Number(5)), 
				ClipboardCollection("bar", BinaryOp("+", Number(5), Number(3))))))
		parsing("@baz(foo(5).bar(5 + 3))") should equal(Function("baz", List(ClipboardRef(List(
				ClipboardCollection("foo", Number(5)), 
				ClipboardCollection("bar", BinaryOp("+", Number(5), Number(3))))))))
		parsing("@baz(foo(5).bar(5 + 3))") should equal(Function("baz", List(ClipboardRef(List(
				ClipboardCollection("foo", Number(5)), 
				ClipboardCollection("bar", BinaryOp("+", Number(5), Number(3))))))))
		parsing("@lol(\"foobar\", 5)") should equal(Function("lol", List(StringLiteral("foobar"), Number(5))))
	}

	
	"The ExpressionParsers" should "parse clipboard references" in {
		//just declare the parser to test once and mark it implicit
		implicit val parserToTest = expr
		parsing("foo.bar.baz") should equal(ClipboardRef(List("foo", "bar", "baz").map{StdClipboardIdent}))
		parsing(".bar.baz") should equal(LocalClipboardRef(List("bar", "baz").map{StdClipboardIdent}))
		assertFail("foo.bar.")
		parsing("@doThings(5 + 5, 5, foo.bar.baz)") should equal(Function("doThings", 
				List(BinaryOp("+", Number(5), Number(5)), Number(5), ClipboardRef(List("foo", "bar", "baz").map{StdClipboardIdent}))))
		parsing("@doThings(5 + 5, 5, .foo.bar.baz)") should equal(Function("doThings", 
				List(BinaryOp("+", Number(5), Number(5)), Number(5), LocalClipboardRef(List("foo", "bar", "baz").map{StdClipboardIdent}))))
		parsing("foo(5).bar") should equal(ClipboardRef(List(ClipboardCollection("foo", Number(5)), StdClipboardIdent("bar"))))
		assertFail("foo(5")
		assertFail("foo 5")
		parsing("foo(5).bar(5 + 3)") should equal(ClipboardRef(List(
				ClipboardCollection("foo", Number(5)), 
				ClipboardCollection("bar", BinaryOp("+", Number(5), Number(3))))))
		parsing("@baz(foo(5).bar(5 + 3))") should equal(Function("baz", List(ClipboardRef(List(
				ClipboardCollection("foo", Number(5)), 
				ClipboardCollection("bar", BinaryOp("+", Number(5), Number(3))))))))
		parsing("@baz(foo(5).bar(5 + 3))") should equal(Function("baz", List(ClipboardRef(List(
				ClipboardCollection("foo", Number(5)), 
				ClipboardCollection("bar", BinaryOp("+", Number(5), Number(3))))))))
	}
	
	"JavaOut" should "generate java code" in {
		implicit val parserToTest = expr
		JavaOut.buildExpr(parsing("5 + 5")) should equal("5.0+5.0")
		JavaOut.buildExpr(parsing("@doThing(5)")) should equal("doThing(5.0)")
		JavaOut.buildExpr(parsing("foo.bar.baz")) should equal("""resolve("foo").resolve("bar").resolve("baz")""")
		JavaOut.buildExpr(parsing("foo(5).bar.baz(5 + 5)")) should equal("""resolve("foo", 5.0).resolve("bar").resolve("baz", 5.0+5.0)""")
	}	
}