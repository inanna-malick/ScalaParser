package parser

sealed trait Value{
    def intValue: Int
    def boolValue: Boolean
    def stringValue: String
}

case class IntValue(literal: Int) extends Value{
    def intValue = literal
    def boolValue   = literal != 0
    def stringValue = literal.toString
    override def toString  = literal.toString
}
    
case class BooleanValue(literal:Boolean) extends Value{
    def intValue = if (literal) 1 else 0
    def boolValue   = literal
    def stringValue = literal.toString
    override def toString  = literal.toString
}
    
case class StringValue(literal: String) extends Value{
    def intValue = literal.toInt
    def boolValue   = if (literal.toLowerCase == "false") false else true
    def stringValue = literal
}

//might want to store pagename here
case class PageValue(contents: Map[String, Value]) extends Value{
    def intValue = throw new UnsupportedOperationException
    def boolValue   = throw new UnsupportedOperationException
    def stringValue = throw new UnsupportedOperationException
}

case class ListValue(contents: Vector[Value]) extends Value{ //0 index
    def intValue = throw new UnsupportedOperationException
    def boolValue   = throw new UnsupportedOperationException
    def stringValue = throw new UnsupportedOperationException
}

case class GroupValue(contents: Map[String, Value]) extends Value{
    def intValue = throw new UnsupportedOperationException
    def boolValue   = throw new UnsupportedOperationException
    def stringValue = throw new UnsupportedOperationException
}