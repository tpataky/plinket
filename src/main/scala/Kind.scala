import io.github.tpataky.duckling.Doc

sealed trait Kind {
  def free: Set[Kind.MVar] =
    this match {
      case Kind.Type        => Set.empty
      case Kind.Record      => Set.empty
      case Kind.Arrow(a, b) => a.free ++ b.free
      case v @ Kind.MVar(_) => Set(v)
    }

  def substitute(v: Kind.MVar, k: Kind): Kind =
    this match {
      case Kind.Type        => this
      case Kind.Record      => this
      case Kind.Arrow(a, b) => Kind.Arrow(a.substitute(v, k), b.substitute(v, k))
      case Kind.MVar(_)     => if (v == this) k else this
    }

  def toDoc: Doc[Unit] = toDoc(Parenthesis.No)

  def toDoc(parenthesis: Parenthesis): Doc[Unit] =
    this match {
      case Kind.Type   => Doc("Type")
      case Kind.Record => Doc("Record")
      case Kind.Arrow(a, b) =>
        val base = a.toDoc(Parenthesis.AroundFun) <\> Doc("->") <\> b.toDoc(Parenthesis.AroundArg)
        parenthesis match {
          case Parenthesis.AroundFun => Doc.lpar <\\> base <\\> Doc.rpar
          case _                     => base
        }
      case Kind.MVar(id) => Doc(StringifyMetaVar(id))
    }
}

object Kind {
  case object Type                   extends Kind
  case object Record                 extends Kind
  case class Arrow(a: Kind, b: Kind) extends Kind
  case class MVar(id: Int)           extends Kind
}
