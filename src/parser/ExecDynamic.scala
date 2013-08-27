package parser

sealed trait Clipboard
case class SingleValue(v: Clipboard) extends Clipboard
case class ValueList(v: Clipboard) extends Clipboard
case class Page(v: Map[String, Clipboard]) extends Clipboard


//all return types are double. TODO: return type with possible values String, Page, Double
object ExecDynamic extends ClipBoardParser{
	
	def apply(s: String): Double = apply(parsing(s)(expr))
	
	def apply(e: Expr): Double = e match {
		case Number(n) => n
		case Parens(e) => apply(e)
		case BinaryOp(op, lhs, rhs) => op match{
			case "+" => apply(lhs) + apply(rhs)
			case "-" => apply(lhs) - apply(rhs)
			case "/" => apply(lhs) / apply(rhs)
			case "*" => apply(lhs) * apply(rhs)
		}
		case Function(f, args) => resolveFunction(f)(args.map(apply))
		case ClipboardRef(refs) => resolveClipboardRef(refs).get
		case LocalClipboardRef(refs) => resolveClipboardRef(refs).get
	}
	
	val a_plus_b: List[Double] => Double = {case List(a, b) => a+b}
	val sum: List[Double] => Double = {(x) => println(s"sum of $x"); x.sum}
	val product: List[Double] => Double = {(x) => println(s"product of $x"); x.product}
	
	val funcs: Map[String, List[Double] => Double] = Map("sum" -> sum, "product" -> product, "a_plus_b" -> a_plus_b)

	//type checking is done at runtime, each function is of signature List(args) => result
	//all args are call by value/strict eval
	def resolveFunction(f: String): List[Double] => Double = {
		funcs.getOrElse(f, throw new NoSuchElementException)
	}
	
	val env1: Map[String, Any] = Map("foo"->Map("bar"->Map("baz"->1.0)))
	
	
	def resolveClipboardRef(refs: List[ClipboardIdent], env: Map[String, Any] = env1): Option[Double] = refs match {
		case h :: t => env.get(h.name).flatMap{ 
				case e: Map[String, Any] => resolveClipboardRef(t, e)
				case e: Double => if (t==Nil) Some(e) 
								else throw new NoSuchElementException
		}
		case Nil =>  throw new NoSuchElementException //clipboard ref must resolve to double for now
	}
}

object app extends Application{
	
	println(ExecDynamic("@sum((5+5 - 2) + foo.bar.baz, 5.0)"))
	println(ExecDynamic("@product(1 + foo.bar.baz, 5.0, 2.0)"))
	println(ExecDynamic("@a_plus_b(1, 2)"))
	println(ExecDynamic("@a_plus_b(1, 2, 3)"))

}