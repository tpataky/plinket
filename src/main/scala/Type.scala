import cats.syntax.foldable._
import cats.syntax.functor._
import io.github.tpataky.duckling.Doc

sealed trait Type {
  def substitute(v: Type, t: Type): Type =
    this match {
      case Type.Str | Type.Num | Type.Bool | Type.Arrow => this
      case Type.App(a, b)                               => Type.App(a.substitute(v, t), b.substitute(v, t))
      case Type.Record(fields, tail) =>
        val substitutedFields = fields.map({ case (s, t0) => (s, t0.substitute(v, t)) })
        val (additionalFields, substitutedTail) =
          tail.map(_.substitute(v, t)) match {
            case t @ Some(_: Type.Var | _: Type.MVar) => Vector.empty[(String, Type)] -> t
            case Some(Type.Record(fields, tail))      => fields                       -> tail
            case Some(_)                              => sys.error("impossible")
            case None                                 => Vector.empty[(String, Type)] -> None
          }
        Type.Record(substitutedFields ++ additionalFields, substitutedTail)
      case Type.Forall(varName, kind, tpe) =>
        // real type checker must check that v isn't bound here
        Type.Forall(varName, kind, tpe.substitute(v, t))
      case Type.Var(_) | Type.MVar(_) => if (this == v) t else this
    }

  def free: Set[Type.MVar] =
    this match {
      case Type.Str | Type.Num | Type.Bool | Type.Arrow | Type.Var(_) => Set.empty
      case Type.App(a, b)                                             => a.free ++ b.free
      case v @ Type.MVar(_)                                           => Set(v)
      case Type.Record(fields, tail) => fields.foldMap(_._2.free) ++ tail.foldMap(_.free)
      case Type.Forall(_, _, tpe)    => tpe.free
    }

  def toDoc: Doc[Unit] = toDoc(Parenthesis.No)

  def toDoc(parenthesis: Parenthesis): Doc[Unit] =
    this match {
      case Type.Str   => Doc("String")
      case Type.Num   => Doc("Num")
      case Type.Bool  => Doc("Boolean")
      case Type.Arrow => Doc("->")
      case Type.App(Type.App(Type.Arrow, a), b) =>
        val base = Doc.sep(a.toDoc(Parenthesis.AroundFun), Doc("->"), b.toDoc(Parenthesis.AroundArg))
        parenthesis match {
          case Parenthesis.AroundFun => base.parens
          case _                     => base
        }
      case Type.App(f, a) =>
        val base = f.toDoc(Parenthesis.AroundFun) <\> a.toDoc(Parenthesis.AroundArg)
        parenthesis match {
          case Parenthesis.AroundArg => base.parens
          case _                     => base
        }
      case Type.Var(name) => Doc(name)
      case Type.MVar(id)  => Doc(StringifyMetaVar(id))
      case Type.Record(fields, tail) =>
        Doc.grouped(Doc.space)(
          Doc
            .intersperse(
              fields.map({ case (f, t) => (Doc(f) + Doc.colon) <\> t.toDoc(Parenthesis.No) }).toList,
              Doc.comma
            )
            .appendedAll(tail.map(t => Doc("... ") + t.toDoc(Parenthesis.No)))
            .tupleLeft(None)
            .appended((Some(0), Doc.rbrace))
            .prepended((Some(0), Doc.lbrace))
        )
      case Type.Forall(varName, _, tpe) =>
        val base = Doc("forall") <\> Doc(varName) <\> Doc.dot <\> tpe.toDoc(Parenthesis.No)
        parenthesis match {
          case Parenthesis.No => base
          case _              => base.parens
        }
    }
}
object Type {
  case object Str                                                       extends Type
  case object Num                                                       extends Type
  case object Bool                                                      extends Type
  case object Arrow                                                     extends Type
  case class App(f: Type, a: Type)                                      extends Type
  case class Var(name: String)                                          extends Type
  case class MVar(id: Int)                                              extends Type
  case class Record(fields: Vector[(String, Type)], tail: Option[Type]) extends Type
  case class Forall(varName: String, kind: Kind, tpe: Type)             extends Type

  def fn(a: Type, b: Type) = Type.App(Type.App(Type.Arrow, a), b)
}
