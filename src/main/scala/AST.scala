import cats.syntax.functor._
import io.github.tpataky.duckling.Doc

sealed trait TypeExpression {
  def toDoc: Doc[Unit] =
    this match {
      case TypeExpression.Id(value)                                           => Doc(value)
      case TypeExpression.Arrow                                               => Doc("->")
      case TypeExpression.App(TypeExpression.App(TypeExpression.Arrow, f), a) => f.toDoc <\> Doc("->") <\> a.toDoc
      case TypeExpression.App(f, a)                                           => f.toDoc <\> a.toDoc
      case TypeExpression.Var(name)                                           => Doc(name)
      case TypeExpression.Record(fields) =>
        Doc.grouped(Doc.space)(
          Doc
            .intersperse(
              fields.map({ case (f, t) => (Doc(f) + Doc.colon) <\> t.toDoc }).toList,
              Doc.comma
            )
            .tupleLeft(None)
            .appended((Some(0), Doc.rbrace))
            .prepended((Some(0), Doc.lbrace))
        )
      case TypeExpression.Forall(v, tpe)     => Doc("forall ") + Doc(v) <+> Doc.dot <\> tpe.toDoc
      case TypeExpression.Parens(expression) => Doc.lpar <\\> expression.toDoc <\\> Doc.rpar
    }
}
object TypeExpression {
  case class Id(value: String)                                extends TypeExpression
  case object Arrow                                           extends TypeExpression
  case class App(f: TypeExpression, a: TypeExpression)        extends TypeExpression
  case class Var(name: String)                                extends TypeExpression
  case class Record(fields: Vector[(String, TypeExpression)]) extends TypeExpression
  case class Forall(v: String, tpe: TypeExpression)           extends TypeExpression
  case class Parens(expression: TypeExpression)               extends TypeExpression
}

sealed trait Expression {
  def toDoc: Doc[Unit] =
    this match {
      case Expression.StringLiteral(value)    => Doc(value).quotes2
      case Expression.BoolLiteral(value)      => Doc(value)
      case Expression.NumberLiteral(value)    => Doc(value)
      case Expression.Var(name)               => Doc(name)
      case Expression.Application(f, a)       => f.toDoc <\> a.toDoc
      case Expression.Parenthesis(expression) => Doc.lpar <\\> expression.toDoc <\\> Doc.rpar
      case Expression.Record(fields) =>
        Doc.grouped(Doc.space)(
          Doc
            .intersperse(
              fields.map({ case (f, e) => (Doc(f) + Doc.equals_) <\> e.toDoc }).toList,
              Doc.comma
            )
            .tupleLeft(None)
            .appended((Some(0), Doc.rbrace))
            .prepended((Some(0), Doc.lbrace))
        )
      case Expression.Projection(record, field)              => record.toDoc + Doc.dot + Doc(field)
      case Expression.Ascription(expression, typeExpression) => expression.toDoc <+> Doc.colon <+> typeExpression.toDoc
      case Expression.Lambda(argName, argType, body) =>
        (Doc("fun ") + Doc(argName) +
          argType.map(t => Doc.colon <+> t.toDoc).getOrElse(Doc.empty) <+> Doc("=>")) <\>
          body.toDoc
      case Expression.Let(varName, tpe, expr, body) =>
        (Doc("let ") + Doc(varName) +
          tpe.map(t => Doc.colon <+> t.toDoc).getOrElse(Doc.empty) <+> Doc("=") <+> expr.toDoc <+> Doc("in")) <\>
          body.toDoc
    }
}
object Expression {
  case class StringLiteral(value: String)                                                          extends Expression
  case class BoolLiteral(value: String)                                                            extends Expression
  case class NumberLiteral(value: String)                                                          extends Expression
  case class Var(name: String)                                                                     extends Expression
  case class Application(f: Expression, a: Expression)                                             extends Expression
  case class Parenthesis(expression: Expression)                                                   extends Expression
  case class Record(fields: Vector[(String, Expression)])                                          extends Expression
  case class Projection(record: Expression, field: String)                                         extends Expression
  case class Ascription(expression: Expression, typeExpression: TypeExpression)                    extends Expression
  case class Lambda(argName: String, argType: Option[TypeExpression], body: Expression)            extends Expression
  case class Let(varName: String, tpe: Option[TypeExpression], expr: Expression, body: Expression) extends Expression
}
