package parser



//runs AST resulting from expression 
class ExecDynamic(functions: Map[String, List[Value] => Value]) extends ClipBoardParser{
	type Env = Map[String, Value]

	def print(s: String)(env: Env): String = {
		val ast: Expr = parsing(s)(expr)
		val r = apply(ast)(env)
		s"input $s => \n\t ast $ast \n\t result $r"
	}
	
	def apply[T >: Value](s: String)(env: Env): T = {
		val ast: Expr = parsing(s)(expr)
		apply(ast)(env)
	}
	
	
	def apply[T >: Value](e: Expr)(env: Env): T = e match {
		case Number(n) => IntValue(n.toInt)
		case StringLiteral(s) => StringValue(s)
		case BooleanLiteral(b) => BooleanValue(b)
		case Parens(e) => apply(e)(env)
		case BinaryOp(op, lhs, rhs) => op match{
			case "+" => IntValue(apply(lhs)(env).intValue + apply(rhs)(env).intValue)
			case "-" => IntValue(apply(lhs)(env).intValue - apply(rhs)(env).intValue)
			case "/" => IntValue(apply(lhs)(env).intValue / apply(rhs)(env).intValue)
			case "*" => IntValue(apply(lhs)(env).intValue * apply(rhs)(env).intValue)
		}
		case Function(f, args) => resolveFunction(f)(args.map(apply(_)(env)))
		case ClipboardRef(refs) => resolveClipboardRef(refs)(env).get
		case LocalClipboardRef(refs) => resolveClipboardRef(refs)(env).get
	}
	
	//functions!	
	def resolveFunction(f: String): List[Value] => Value = {
		functions.getOrElse(f, throw new UnsupportedOperationException(s"no function $f exists"))
	}
	

	//println(s"refs => $refs, env => $env"); 
	def resolveClipboardRef[T](refs: List[ClipboardIdent])(env: Env): Option[Value] = refs match {
		case StdClipboardIdent(name) :: t => env.get(name).flatMap{v => if (t==Nil) Some(v) else v match {
				case PageValue(m) => resolveClipboardRef(t)(m)
				case v: Value => throw new NoSuchElementException("clipboard element not found") //terms to resolve, but to what do I apply them?
		}}
		case ClipboardCollection(name, key) :: t => env.get(name).flatMap{v => 
			val r = (v, apply(key)(env)) match {
				case (ListValue(content), IntValue(i)) => content(i)
				case (GroupValue(content), StringValue(s)) => content.getOrElse(s, throw new NoSuchElementException("group element not found"))
				case v: Value => throw new NoSuchElementException("clipboard element not found") //terms to resolve, but to what do I apply them?
			}
			if (t==Nil ) Some(r) else r match {
				case PageValue(m) => resolveClipboardRef(t)(m)
				case v: Value => throw new NoSuchElementException("clipboard element not found") //terms to resolve, but to what do I apply them?
			}
		}
		case Nil =>  throw new IllegalArgumentException("attempted to resolve empty list of ClipboardIdent's")
	}
}


object ExecTests extends Application{
		val env: Map[String, Value] = 
		Map("foo"->
			PageValue(Map("bar"->
				PageValue(Map("baz"->
					IntValue(1)
				))
			)),
			"page1" -> GroupValue(Map("key"-> PageValue(Map("page2"->
				ListValue(Vector(
					IntValue(1), 
					IntValue(2)
				))))
			))
		)
	
	val get: List[Value] => Value = {case List(PageValue(m), StringValue(s)) => m(s)}
	val silly_string: List[Value] => Value = 
			{case s :: Nil => 
				val str = s.stringValue; 
				StringValue(s"lol wut is this: $str")
			}
	val and: List[Value] => Value = {case List(a, b) => BooleanValue(a.boolValue && b.boolValue)}
	val a_plus_b: List[Value] => Value = {case List(a, b) => IntValue(a.intValue+b.intValue)}
	val sum: List[Value] => Value = (x) => IntValue(x.map(_.intValue).sum)
	val product: List[Value] => Value = (x) => IntValue(x.map(_.intValue).product)
	
	val functions: Map[String, List[Value] => Value] = 
		Map("sum" -> sum, 
			"product" -> product, 
			"a_plus_b" -> a_plus_b,
			"silly_string" -> silly_string,
			"get" -> get,
			"and" -> and
		)
	
	val context = new ExecDynamic(functions)
	
	
	// some numeric functions
	println(context.print("(5-5)-3")(env))
	println(context.print("@sum((5+5 - 2) + foo.bar.baz, 5.0)")(env))
	println(context.print("@product(1 + foo.bar.baz, 5.0, 2.0)")(env))
	println(context.print("@a_plus_b(1, 2)")(env))
	
	// lets inspect the clipboard
	println(context.print("foo.bar.baz")(env))
	println(context.print("foo.bar")(env))
	println(context.print("foo")(env))

	// functions with strings
	println(context.print("@silly_string(\"check this out\")")(env))
	
	// let's write our own get(src_page, key) function, also nesting functions
	println(context.print("@silly_string(@get(foo.bar, \"baz\"))")(env))
	
	// boolean functions
	println(context.print("@and(true, true)")(env))
	println(context.print("@and(true, @and(true, @and(true, @and(true, @and(true, @and(true, @and(true, true)))))))")(env))
	println(context.print("@and(false, true)")(env))	
	println(context.print("@and(false, false)")(env))
	
	
	// group/list clipboard access
	println(context.print("page1(\"key\").page2(1)")(env))
	println(context.print("page1(\"key\").page2(0) + page1(\"key\").page2(1)")(env))
	
	val x: parser.ExecTests.context.Env => parser.Value = context("page1(\"key\").page2(0) + page1(\"key\").page2(1)")_
	println(x)

}