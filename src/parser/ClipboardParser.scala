package parser

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.PackratParsers
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers



// note: packratparsers is the full recursive expression special sauce
// note: ident allows for java keywords like null, any [java-letter][java-letter/number]*
class ClipBoardParser extends JavaTokenParsers with PackratParsers {
	lazy val clipboardIdent: PackratParser[ClipboardIdent] =
		(ident~"("~expr~")") ^^ {case c~"("~e~")" => ClipboardCollection(c, e)} |
		ident ^^ {StdClipboardIdent}

	lazy val clipboardRef: PackratParser[Expr] =
		rep1sep(clipboardIdent, ".") ^^ { refs => ClipboardRef(refs) } |
		rep1("."~>clipboardIdent) ^^ { refs => LocalClipboardRef(refs) }
	
	lazy val op: PackratParser[String] = 
		"+" | "-" | "*" | "/"
	
	lazy val binaryOp: PackratParser[Expr] = 
		expr~op~expr ^^ {case lhs~op~rhs => BinaryOp(op, lhs, rhs)}		
		
	lazy val expr: PackratParser[Expr] =
		binaryOp | //order matters, switch this and factor and all fail
		func | clipboardRef |
		parens |
		literal

	lazy val parens: PackratParser[Expr] = 
		"("~>expr<~")" ^^ {e => Parens(e)}
		
	lazy val func: PackratParser[Expr] =  
		("@"~ident~"("~repsep(expr, ",")~")") ^^ {
			case "@"~f~"("~args~")" => Function(f, args)
		}

	lazy val literal: PackratParser[Expr] =
		floatingPointNumber ^^ { x => Number(x.toFloat) } |
		stringLiteral ^^ { s => StringLiteral(s.drop(1).dropRight(1))}
		
	
	protected def parsing[T](s: String)(implicit p: Parser[T]): T = {
		parseAll(p, s) match {
			case Success(t, _) =>  t
			case NoSuccess(msg, _) => throw new IllegalArgumentException(
				"Could not parse '" + s + "': " + msg)
		}
	}
	
}

class ClipBoardParsersTest extends ClipBoardParser with FlatSpec with ShouldMatchers {

	private def assertFail[T](input: String)(implicit p: Parser[T]) {
		evaluating(parsing(input)) should produce[IllegalArgumentException]
	}


	"The ExpressionParsers" should "parse simple expressions" in {
		//just declare the parser to test once and mark it implicit
		implicit val parserToTest = expr
		parsing("15") should equal(Number(15))
		parsing("5 + 5") should equal(BinaryOp("+", Number(5), Number(5)))		
		parsing("(5 + 5)") should equal(Parens(BinaryOp("+", Number(5), Number(5))))		
		assertFail("5 +")
		parsing("(5 + 5) + 5") should equal(BinaryOp("+", Parens(BinaryOp("+", Number(5), Number(5))), Number(5)))
		assertFail("5 + (5 + 5")
		parsing("(5 + 5) + (5 + 5)") should equal(BinaryOp("+", Parens(BinaryOp("+", Number(5), Number(5))), Parens(BinaryOp("+", Number(5), Number(5)))))
		parsing("(5 + 5) + (5 - 5)") should equal(BinaryOp("+", Parens(BinaryOp("+", Number(5), Number(5))), Parens(BinaryOp("-", Number(5), Number(5)))))
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
		JavaOut.buildExpr(parsing("5 + 5")) should equal("5.0 + 5.0")
		JavaOut.buildExpr(parsing("@doThing(5)")) should equal("doThing(5.0)")
		JavaOut.buildExpr(parsing("foo.bar.baz")) should equal("""resolve("foo").resolve("bar").resolve("baz")""")
		JavaOut.buildExpr(parsing("foo(5).bar.baz(5 + 5)")) should equal("""resolve("foo", 5.0).resolve("bar").resolve("baz", 5.0 + 5.0)""")
	}	
}




//all return types are double. TODO: return type with possible values String, Page, Double
object ExecDynamic extends ClipBoardParser{
	
	def apply[T](s: String): String = {
		val r = apply(parsing(s)(expr))
		s"$s => $r"
	}
	
	def apply[T](e: Expr): Value = e match {
		case Number(n) => NumberValue(n)
		case StringLiteral(s) => StringValue(s)
		case Parens(e) => apply(e)
		case BinaryOp(op, lhs, rhs) => op match{
			case "+" => NumberValue(apply(lhs).doubleValue + apply(rhs).doubleValue)
			case "-" => NumberValue(apply(lhs).doubleValue - apply(rhs).doubleValue)
			case "/" => NumberValue(apply(lhs).doubleValue / apply(rhs).doubleValue)
			case "*" => NumberValue(apply(lhs).doubleValue * apply(rhs).doubleValue)
		}
		case Function(f, args) => resolveFunction(f)(args.map(apply(_)))
		case ClipboardRef(refs) => resolveClipboardRef(refs).get
		case LocalClipboardRef(refs) => resolveClipboardRef(refs).get
	}
	
	val silly_string: List[Value] => Value = 
			{case s :: Nil => 
				val s2 = s.stringValue; 
				StringValue(s"lol wut is this: $s2")
			}
	
	val a_plus_b: List[Value] => Value = {case List(a, b) => NumberValue(a.doubleValue+b.doubleValue)}
	val sum: List[Value] => Value = {(x) => println(s"sum of $x"); NumberValue(x.map(_.doubleValue).sum)}
	val product: List[Value] => Value = {(x) => println(s"product of $x"); NumberValue(x.map(_.doubleValue).product)}
	
	val funcs: Map[String, List[Value] => Value] = 
		Map("sum" -> sum, 
			"product" -> product, 
			"a_plus_b" -> a_plus_b,
			"silly_string" -> silly_string
		)

	//type checking is done at runtime, each function is of signature List(args) => result
	//all args are call by value/strict eval
	def resolveFunction(f: String): List[Value] => Value = {
		funcs.getOrElse(f, throw new UnsupportedOperationException(s"no function $f exists"))
	}
	
	val env1: Map[String, ClipboardEntity] = 
		Map("foo"->
			ClipboardEntity(PageValue(Map("bar"->
				ClipboardEntity(PageValue(Map("baz"->
					ClipboardEntity(NumberValue(1.0))
				)))
			)))
		)
	
	
	def resolveClipboardRef[T](refs: List[ClipboardIdent], env: Map[String, ClipboardEntity] = env1): Option[Value] = refs match {
		case h :: t => env.get(h.name).map(_.v).flatMap{v => if (t==Nil) Some(v) else v match {
				case PageValue(m) => resolveClipboardRef(t, m)
				case v: Value => throw new NoSuchElementException("clipboard element not found") //terms to resolve, but to what do I apply them?
		}}
		case Nil =>  throw new IllegalArgumentException("attempted to resolve empty list of ClipboardIdent's")
	}
}

case class ClipboardEntity(v: Value)

sealed trait Value{
    def doubleValue: Double
    def boolValue: Boolean
    def stringValue: String
}

case class NumberValue(literal:Double) extends Value{
    def doubleValue = literal
    def boolValue   = literal != 0.0
    def stringValue = literal.toString
    override def toString  = literal.toString
}
    
case class BooleanValue(literal:Boolean) extends Value{
    def doubleValue = if (literal) 1.0 else 0.0
    def boolValue   = literal
    def stringValue = literal.toString
    override def toString  = literal.toString
}
    
case class StringValue(literal: String) extends Value{
    def doubleValue = literal.toDouble
    def boolValue   = if (literal.toLowerCase == "false") false else true
    def stringValue = literal
}

case class PageValue(contents: Map[String, ClipboardEntity]) extends Value{
    def doubleValue = throw new UnsupportedOperationException
    def boolValue   = throw new UnsupportedOperationException
    def stringValue = throw new UnsupportedOperationException
}

object ExecTests extends Application{
	//some numeric functions
	println(ExecDynamic("@sum((5+5 - 2) + foo.bar.baz, 5.0)"))
	println(ExecDynamic("@product(1 + foo.bar.baz, 5.0, 2.0)"))
	println(ExecDynamic("@a_plus_b(1, 2)"))
	
	//lets inspect the clipboard
	println(ExecDynamic("foo.bar.baz"))
	println(ExecDynamic("foo.bar"))
	println(ExecDynamic("foo"))

	println(ExecDynamic("@silly_string(\"check this out\")"))	
}
