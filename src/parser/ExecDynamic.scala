package parser

object F {
	val get: List[Value] => Value = { case List(PageValue(m), StringValue(s)) => m(s) }
	val silly_string: List[Value] => Value =
		{
			case s :: Nil =>
				val str = s.stringValue;
				StringValue(s"lol wut is this: $str")
		}
	val and: List[Value] => Value = { case List(a, b) => BooleanValue(a.boolValue && b.boolValue) }
	val a_plus_b: List[Value] => Value = { case List(a, b) => IntValue(a.intValue + b.intValue) }
	val sum: List[Value] => Value = (x) => IntValue(x.map(_.intValue).sum)
	val product: List[Value] => Value = (x) => IntValue(x.map(_.intValue).product)

	val split: List[Value] => Value = {case StringValue(s) :: Nil => ListValue(s.split(' ').map(StringValue))}

	
	val functions: Map[String, List[Value] => Value] =
		Map("sum" -> sum,
			"product" -> product,
			"a_plus_b" -> a_plus_b,
			"silly_string" -> silly_string,
			"get" -> get,
			"and" -> and,
			"split"->split)
			
}

//runs AST resulting from expression 
object ExecDynamic extends ClipBoardParser {
	type Env = Map[String, Value]

	def print(s: String)(env: Env): String = {
		val ast: Expr = parsing(s)(expr)
		val r = apply(ast)(env)
		s"input $s => \n\t ast $ast \n\t result $r"
	}

	def apply(s: String)(env: Env): Value = {
		val ast: Expr = parsing(s)(expr)
		apply(ast)(env)
	}

	def apply(e: Expr)(env: Env): Value = e match {
		case Number(n) => IntValue(n.toInt)
		case StringLiteral(s) => StringValue(s)
		case BooleanLiteral(b) => BooleanValue(b)
		case Parens(e) => apply(e)(env)
		case BinaryOp(op, lhs, rhs) => op match {
			case "+" => IntValue(apply(lhs)(env).intValue + apply(rhs)(env).intValue)
			case "-" => IntValue(apply(lhs)(env).intValue - apply(rhs)(env).intValue)
			case "/" => IntValue(apply(lhs)(env).intValue / apply(rhs)(env).intValue)
			case "*" => IntValue(apply(lhs)(env).intValue * apply(rhs)(env).intValue)
		}
		case Function(f, args) => resolveFunction(f)(args.map(apply(_)(env)))
		case ClipboardRef(refs) => resolveClipboardRef(refs)(env)
		case LocalClipboardRef(refs) => resolveClipboardRef(refs)(env)
	}

	//functions!	
	def resolveFunction(f: String): List[Value] => Value = {
		F.functions.getOrElse(f, throw new UnsupportedOperationException(s"no function $f exists"))
	}

	def resolveClipboardRef[T](refs: List[ClipboardIdent])(env: Env): Value = refs match {
		case StdClipboardIdent(name) :: t => env.get(name).flatMap { v =>
			if (t == Nil) Some(v) else v match {
				case PageValue(m) => Some(resolveClipboardRef(t)(m))
				case v: Value => throw new NoSuchElementException("clipboard element not found") //terms to resolve, but to what do I apply them?
			}
		}.getOrElse(throw new NoSuchElementException("clipboard element not found"))
		case ClipboardCollection(name, key) :: t => env.get(name).flatMap { v =>
			val r = (v, apply(key)(env)) match {
				case (ListValue(content), IntValue(i)) => content(i)
				case (GroupValue(content), StringValue(s)) => content.getOrElse(s, throw new NoSuchElementException("group element not found"))
				case v: Value => throw new NoSuchElementException("clipboard element not found") //terms to resolve, but to what do I apply them?
			}
			if (t == Nil) Some(r) else r match {
				case PageValue(m) => Some(resolveClipboardRef(t)(m))
				case v: Value => throw new NoSuchElementException("clipboard element not found") //terms to resolve, but to what do I apply them?
			}
		}.getOrElse(throw new NoSuchElementException("clipboard element not found"))
		case Nil => throw new IllegalArgumentException("attempted to resolve empty list of ClipboardIdent's")
	}
}

object ExecTests extends Application {
	val env: Map[String, Value] =
		Map("foo" ->
			PageValue(Map("bar" ->
				PageValue(Map("baz" ->
					IntValue(1))))),
			"page1" -> GroupValue(Map("key" -> PageValue(Map("page2" ->
				ListValue(Vector(
					IntValue(1),
					IntValue(2))))))))
		
	// some numeric functions
	println(ExecDynamic.print("(5-5)-3")(env))
	println(ExecDynamic.print("@sum((5+5 - 2) + foo.bar.baz, 5.0)")(env))
	println(ExecDynamic.print("@product(1 + foo.bar.baz, 5.0, 2.0)")(env))
	println(ExecDynamic.print("@a_plus_b(1, 2)")(env))

	// lets inspect the clipboard
	println(ExecDynamic.print("foo.bar.baz")(env))
	println(ExecDynamic.print("foo.bar")(env))
	println(ExecDynamic.print("foo")(env))

	// functions with strings
	println(ExecDynamic.print("@silly_string(\"check this out\")")(env))

	// let's write our own get(src_page, key) function, also nesting functions
	println(ExecDynamic.print("@silly_string(@get(foo.bar, \"baz\"))")(env))

	// boolean functions
	println(ExecDynamic.print("@and(true, true)")(env))
	println(ExecDynamic.print( //10 deep nested @and(true, true)
			(1 to 10).foldLeft("true")(
					(acc: String, _) => s"@and(true, $acc)"
			))(env))
	println(ExecDynamic.print("@and(false, true)")(env))
	println(ExecDynamic.print("@and(false, false)")(env))

	// group/list clipboard access
	println(ExecDynamic.print("page1(\"key\").page2(1)")(env))
	println(ExecDynamic.print("page1(\"key\").page2(0) + page1(\"key\").page2(1)")(env))

}