package parser

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.PackratParsers



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
	
	lazy val binaryOp: PackratParser[BinaryOp] = 
		expr~op~expr ^^ {case lhs~op~rhs => BinaryOp(op, lhs, rhs)}		
		
	lazy val expr: PackratParser[Expr] =
		binaryOp | //order matters, switch this and factor and all fail
		parens |
		literal |
		func | 
		clipboardRef
		

	lazy val parens: PackratParser[Parens] = 
		"("~>expr<~")" ^^ {e => Parens(e)}
		
	lazy val func: PackratParser[Function] =  
		("@"~ident~"("~repsep(expr, ",")~")") ^^ {
			case "@"~f~"("~args~")" => Function(f, args)
		}

	lazy val boolean: PackratParser[BooleanLiteral] =
		("true" | "false") ^^ { b => BooleanLiteral(b == "true") }

	lazy val string: PackratParser[StringLiteral] =
		stringLiteral ^^ { s => StringLiteral(s.drop(1).dropRight(1)) }

	lazy val float: PackratParser[Number] =
		floatingPointNumber ^^ { x => Number(x.toFloat) }

	
	lazy val literal: PackratParser[Expr] =
		boolean |
		string |
		float
		
	
	protected def parsing[T](s: String)(implicit p: Parser[T]): T =
		parseAll(p, s) match {
			case Success(t, _) =>  t
			case NoSuccess(msg, _) => 
				throw new IllegalArgumentException(s"Could not parse '$s' $msg")
		}

	
}
