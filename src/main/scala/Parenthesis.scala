sealed trait Parenthesis
object Parenthesis {
  case object No        extends Parenthesis
  case object AroundFun extends Parenthesis
  case object AroundArg extends Parenthesis
}
