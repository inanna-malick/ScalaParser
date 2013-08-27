package parser

object JavaOut extends ClipBoardParser{
	def buildExpr(e: Expr): String = e match { 
		case Number(n) => n.toString
		case StringLiteral(s) => s""""$s"""""
		case Parens(e) => "(" + buildExpr(e) + ")"
		case BinaryOp(op, lhs, rhs) => buildExpr(lhs) + " " + op + " " + buildExpr(rhs)
		case Function(f, args) => f + "(" + args.map{buildExpr}.mkString(",") + ")"
		case ClipboardRef(refs) => refs.map{buildRef}.mkString(".")
		case LocalClipboardRef(refs) => refs.map{buildRef}.mkString(".") //whatever, let's just simplify this. local is always in frame
	}
	
	def buildRef(c: ClipboardIdent): String = c match {
		case StdClipboardIdent(n) => s"""resolve("$n")"""
		case ClipboardCollection(n, expr) => val e = buildExpr(expr); s"""resolve("$n", $e)"""
	}

	def apply(s: String): String = parseAll(expr, s) match {
			case Success(t, _) =>  JavaOut.buildExpr(t)
			case NoSuccess(msg, _) => throw new IllegalArgumentException(
				"Could not parse '" + s + "': " + msg)
		} 
}