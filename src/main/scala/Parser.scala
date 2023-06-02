import cats.data.NonEmptyList
import cats.parse.{Parser => P, Parser0 => P0}

object Parser {

  val ws: P0[String] = P.charsWhile0(_.isWhitespace)

  val typeName: P[TypeExpression] = (P.charWhere(_.isUpper) ~ P.charsWhile(_.isLetter)).string.map(TypeExpression.Id(_))

  val typeVar: P[TypeExpression.Var] = P.charsWhile(_.isLetter).map(TypeExpression.Var(_))

  val typeArrow: P[TypeExpression] = P.string("->").as(TypeExpression.Arrow)

  val recordType: P[TypeExpression] =
    P.char('{') *> ws *>
      ((P.charsWhile(_.isLetter) <* ws <* P.char(':') <* ws) ~ P.defer(typeExpression))
        .repSep0(ws *> P.char(',') *> ws)
        .map(fields => TypeExpression.Record(fields.toVector)) <*
      ws <* P.char('}')

  val forall: P[TypeExpression] =
    ((P.string("forall ") *> ws *> typeVar <* ws <* P.char('.') <* ws) ~ P.defer(typeExpression))
      .map({ case (v, t) => TypeExpression.Forall(v.name, t) })

  val parensType: P[TypeExpression] = P.char('(') *> ws *> P.defer(typeExpression) <* ws <* P.char(')')

  lazy val typeExpression: P[TypeExpression] = {
    val simple = recordType | parensType | forall | typeArrow | typeName | typeVar

    simple.repSep(ws).map(buildTypeApplication)
  }

  val stringLiteral: P[Expression] =
    P.char('"') *> P.charsWhile(_ != '"').map(Expression.StringLiteral(_)) <* P.char('"')

  val boolLiteral: P[Expression] = (P.string("true") | P.string("false")).string.map(Expression.BoolLiteral(_))

  val numLiteral: P[Expression] = cats.parse.Numbers.digits.map(Expression.NumberLiteral(_))

  val keywords = Set("fun", "let", "in")
  val variable: P[Expression.Var] =
    P.charsWhile(_.isLetter).filter(s => !keywords.contains(s)).backtrack.map(Expression.Var(_))

  val parenthesis: P[Expression] = P.char('(') *> ws *> P.defer(expression) <* ws <* P.char(')')

  val record: P[Expression] =
    P.char('{') *> ws *>
      ((P.charsWhile(_.isLetter) <* ws <* P.char('=') <* ws) ~ P.defer(expression))
        .repSep0(ws *> P.char(',') *> ws)
        .map(fields => Expression.Record(fields.toVector)) <*
      ws <* P.char('}')

  val lambda: P[Expression] =
    (((P.string("fun ") *> ws *> variable <* ws) ~ (P.char(':') *> ws *> typeExpression).?) ~
      (ws *> P.string("=>") *> ws *> P.defer(expression)))
      .map({ case ((v, tpe), body) => Expression.Lambda(v.name, tpe, body) })

  val let: P[Expression] =
    (((P.string("let ") *> ws *> variable <* ws) ~ (P.char(':') *> ws *> typeExpression).?) ~
      (ws *> P.char('=') *> ws *> P.defer(expression)) ~
      (ws *> P.string("in") *> ws *> P.defer(expression)))
      .map({ case (((v, t), expr), body) => Expression.Let(v.name, t, expr, body) })

  val simple = stringLiteral | boolLiteral | numLiteral | parenthesis | record | lambda | let | variable

  lazy val expression: P[Expression] = {
    val projections =
      (simple ~ (P.char('.') *> P.charsWhile(_.isLetter).repSep(P.char('.'))).?)
        .map({
          case (e, Some(fields)) => fields.foldLeft(e)((r, f) => Expression.Projection(r, f))
          case (e, None)         => e
        })

    val applications =
      projections
        .repSep(ws)
        .map(es => es.tail.foldLeft(es.head)((f, a) => Expression.Application(f, a)))

    (applications ~ (ws.soft *> P.char(':') *> ws *> typeExpression).?).map({
      case (e, Some(t)) => Expression.Ascription(e, t)
      case (e, None)    => e
    })
  }

  def buildTypeApplication(ne: NonEmptyList[TypeExpression]): TypeExpression = {
    def go(hd: TypeExpression, in: List[TypeExpression], stack: List[TypeExpression]): TypeExpression = {
      (in, stack) match {
        case (Nil, Nil)                           => hd
        case (Nil, a :: tail)                     => go(TypeExpression.App(a, hd), Nil, tail)
        case (TypeExpression.Arrow :: Nil, _)     => go(TypeExpression.App(TypeExpression.Arrow, hd), Nil, stack)
        case (TypeExpression.Arrow :: a :: tl, _) => go(a, tl, TypeExpression.App(TypeExpression.Arrow, hd) :: stack)
        case (a :: tl, _)                         => go(TypeExpression.App(hd, a), tl, stack)
      }
    }

    go(ne.head, ne.tail, Nil)
  }

}
