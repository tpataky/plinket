import cats.{Comparison, Order}
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._

import scala.annotation.tailrec

object TypeCheck {

  def checkKind(typeExpression: TypeExpression, expected: Kind): Contextual[Type] = {
    val action = typeExpression match {
      case TypeExpression.Id(value) =>
        value match {
          case "String"  => unifies(Kind.Type, expected).as(Type.Str)
          case "Boolean" => unifies(Kind.Type, expected).as(Type.Bool)
          case "Num"     => unifies(Kind.Type, expected).as(Type.Num)
          case _         => TypeError(s"Unknown type $value").raiseError[Contextual, Type]
        }
      case TypeExpression.Arrow =>
        unifies(Kind.Arrow(Kind.Type, Kind.Arrow(Kind.Type, Kind.Type)), expected).as(Type.Arrow)
      case TypeExpression.App(_, _) =>
        inferKind(typeExpression).flatMap({ case (t, inferred) => unifies(inferred, expected).as(t) })
      case TypeExpression.Var(name) =>
        Contextual
          .lookupTypeVar(name)
          .flatMap({
            case Some(k) => unifies(k, expected).as(Type.Var(name))
            case None    => TypeError(s"Unknown type var $name").raiseError[Contextual, Type]
          })
      case TypeExpression.Record(fields) =>
        for {
          checkedFields <- fields.traverse({ case (field, t) => checkKind(t, Kind.Type).tupleLeft(field) })
          _             <- unifies(Kind.Record, expected)
        } yield Type.Record(checkedFields, None)
      case TypeExpression.Forall(v, tpe) =>
        for {
          alpha <- Contextual.freshKindMeta
          _     <- Contextual.restore(Entry.Marker, Entry.KindMetaVar(alpha), Entry.Kinding(v, alpha))
          t     <- checkKind(tpe, expected)
          solved <- Contextual.get.map(ctx =>
            ctx.apply(alpha) match {
              case Kind.MVar(_) => Kind.Type
              case k            => k
            }
          )
          _ <- Contextual.stripTail
        } yield Type.Forall(v, solved, t)
      case TypeExpression.Parens(expression) => checkKind(expression, expected)
    }
    action.traced(Activity.CheckKind(typeExpression, expected))
  }

  def inferKind(typeExpression: TypeExpression): Contextual[(Type, Kind)] = {
    val action = typeExpression match {
      case TypeExpression.Id(value) =>
        value match {
          case "String"  => (Type.Str, Kind.Type).pure[Contextual]
          case "Boolean" => (Type.Bool, Kind.Type).pure[Contextual]
          case "Num"     => (Type.Num, Kind.Type).pure[Contextual]
          case _         => TypeError(s"Unknown type $value").raiseError[Contextual, (Type, Kind)]
        }
      case TypeExpression.Arrow =>
        (Type.Arrow, Kind.Arrow(Kind.Type, Kind.Arrow(Kind.Type, Kind.Type))).pure[Contextual]
      case TypeExpression.App(f, a) =>
        inferKind(f).flatMap({
          case (ft, Kind.Arrow(a1, a2)) => checkKind(a, a1).map(at => (Type.App(ft, at), a2))
          case (ft, v @ Kind.MVar(_)) =>
            for {
              alpha1 <- Contextual.freshKindMeta
              alpha2 <- Contextual.freshKindMeta
              _      <- Contextual.restore(Entry.KindMetaVar(alpha1), Entry.KindMetaVar(alpha2))
              _      <- unifies(v, Kind.Arrow(alpha1, alpha2))
              at     <- checkKind(a, alpha1)
              solved <- Contextual.get.map(ctx => ctx.apply(alpha2))
            } yield Type.App(ft, at) -> solved
          case _ => TypeError("Kind error").raiseError[Contextual, (Type, Kind)]
        })
      case TypeExpression.Var(name) =>
        Contextual
          .lookupTypeVar(name)
          .flatMap({
            case Some(kind) => (Type.Var(name), kind).pure[Contextual]
            case None       => TypeError(s"Unknown type var: $name").raiseError[Contextual, (Type, Kind)]
          })
      case TypeExpression.Record(fields) =>
        fields
          .traverse({ case (field, te) => checkKind(te, Kind.Type).tupleLeft(field) })
          .map(fields => (Type.Record(fields, None), Kind.Record))
      case TypeExpression.Forall(v, tpe) =>
        for {
          alpha <- Contextual.freshKindMeta
          _     <- Contextual.restore(Entry.Kinding(v, alpha))
          tpe   <- checkKind(tpe, Kind.Type)
          solved <- Contextual.get.map(ctx =>
            ctx.apply(alpha) match {
              case Kind.MVar(_) => Kind.Type
              case k            => k
            }
          )
          _ <- Contextual.stripFromVar(v)
        } yield Type.Forall(v, solved, tpe) -> Kind.Type
      case TypeExpression.Parens(expression) => inferKind(expression)
    }

    action.traced(Activity.InferKind(typeExpression))
  }

  def checkType(expression: Expression, expected: Type): Contextual[Unit] = {
    val action = (expression, expected) match {
      case (_, Type.Forall(v, k, t)) =>
        Contextual.restore(Entry.Kinding(v, k)) >>
          checkType(expression, t) >>
          Contextual.stripFromVar(v)
      case (Expression.Parenthesis(e), _)   => checkType(e, expected)
      case (Expression.StringLiteral(_), _) => unifies(Type.Str, expected)
      case (Expression.BoolLiteral(_), _)   => unifies(Type.Bool, expected)
      case (Expression.NumberLiteral(_), _) => unifies(Type.Num, expected)
      case (Expression.Var(name), _) =>
        Contextual
          .lookupVar(name)
          .flatMap({
            case Some(tpe) => unifies(tpe, expected)
            case None      => TypeError(s"Unknown variable $name").raiseError[Contextual, Unit]
          })

      case (Expression.Ascription(expression, typeExpression), _) =>
        for {
          t <- checkKind(typeExpression, Kind.Type)
          _ <- checkType(expression, t)
          _ <- unifies(t, expected)
        } yield ()
      case (Expression.Lambda(argName, argType, body), Type.App(Type.App(Type.Arrow, a), b)) =>
        for {
          t <- argType.traverse(checkKind(_, Kind.Type))
          _ <- t match {
            case Some(t) =>
              unifies(a, t) >>
                Contextual.restore(Entry.Typing(argName, t)) >>
                checkType(body, b) >>
                Contextual.stripFromVar(argName)
            case None =>
              Contextual.restore(Entry.Typing(argName, a)) >> checkType(body, b) >> Contextual.stripFromVar(argName)
          }
        } yield ()
      case (Expression.Let(varName, varType, expr, body), _) =>
        for {
          t <- varType.traverse(checkKind(_, Kind.Type))
          _ <- t match {
            case Some(t) =>
              checkType(expr, t) >> Contextual.restore(Entry.Typing(varName, t)) >>
                checkType(body, expected) >> Contextual.stripFromVar(varName)
            case None =>
              for {
                _        <- Contextual.restore(Entry.Marker)
                t        <- inferType(expr)
                unsolved <- Contextual.stripTail
                generalised = unsolved.foldLeft(t)({ case (t, mv) =>
                  if (t.free.contains(mv.v)) {
                    val varName = StringifyMetaVar(mv.v.id)
                    Type.Forall(varName, mv.kind, t.substitute(mv.v, Type.Var(varName)))
                  } else {
                    t
                  }
                })
                _ <- Contextual.restore(Entry.Typing(varName, generalised)) >>
                  checkType(body, expected) >>
                  Contextual.stripFromVar(varName)
              } yield ()
          }
        } yield ()
      case (Expression.Projection(record, field), _) =>
        for {
          rho <- Contextual.freshTypeMeta
          _   <- Contextual.restore(Entry.TypeMetaVar(rho, Kind.Record))
          _   <- checkType(record, Type.Record(Vector(field -> expected), Some(rho)))
        } yield ()
      case (Expression.Lambda(_, _, _) | Expression.Record(_) | Expression.Application(_, _), _) =>
        inferType(expression).flatMap(inferred => unifies(inferred, expected))
    }

    action.traced(Activity.CheckType(expression, expected))
  }

  def inferType(expression: Expression): Contextual[Type] = {
    val action = expression match {
      case Expression.Parenthesis(e)   => inferType(e)
      case Expression.StringLiteral(_) => Type.Str.pure[Contextual]
      case Expression.BoolLiteral(_)   => Type.Bool.pure[Contextual]
      case Expression.NumberLiteral(_) => Type.Num.pure[Contextual]
      case Expression.Var(name) =>
        Contextual
          .lookupVar(name)
          .flatMap({
            case Some(tpe) => tpe.pure[Contextual]
            case None      => TypeError(s"Unknown var $name").raiseError[Contextual, Type]
          })
      case Expression.Application(f, a) => inferType(f).flatMap(funType => inferApply(funType, a))
      case Expression.Record(fields) =>
        fields
          .traverse({ case (field, e) => inferType(e).tupleLeft(field) })
          .map({ fields => Type.Record(fields, None) })
      case Expression.Ascription(expression, typeExpression) =>
        for {
          t <- checkKind(typeExpression, Kind.Type)
          _ <- checkType(expression, t)
        } yield t
      case Expression.Lambda(argName, argType, body) =>
        for {
          t <- argType.traverse(checkKind(_, Kind.Type))
          out <- t match {
            case Some(t) =>
              for {
                _ <- Contextual.restore(Entry.Typing(argName, t))
                b <- inferType(body)
                _ <- Contextual.stripFromVar(argName)
              } yield Type.App(Type.App(Type.Arrow, t), b)
            case None =>
              for {
                alpha <- Contextual.freshTypeMeta
                _     <- Contextual.restore(Entry.TypeMetaVar(alpha, Kind.Type), Entry.Typing(argName, alpha))
                b     <- inferType(body)
                _     <- Contextual.stripFromVar(argName)
              } yield Type.App(Type.App(Type.Arrow, alpha), b)
          }
        } yield out
      case Expression.Let(varName, varType, expr, body) =>
        for {
          t <- varType.traverse(checkKind(_, Kind.Type))
          out <- t match {
            case Some(t) =>
              checkType(expr, t) >> Contextual.restore(Entry.Typing(varName, t)) >>
                inferType(body) <* Contextual.stripFromVar(varName)
            case None =>
              for {
                _        <- Contextual.restore(Entry.Marker)
                t        <- inferType(expr)
                unsolved <- Contextual.stripTail
                generalised = unsolved.foldLeft(t)({ case (t, mv) =>
                  if (t.free.contains(mv.v)) {
                    val varName = StringifyMetaVar(mv.v.id)
                    Type.Forall(varName, mv.kind, t.substitute(mv.v, Type.Var(varName)))
                  } else {
                    t
                  }
                })
                out <- Contextual.restore(Entry.Typing(varName, generalised)) >>
                  inferType(body) <*
                  Contextual.stripFromVar(varName)
              } yield out
          }
        } yield out
      case Expression.Projection(record, field) =>
        for {
          rho   <- Contextual.freshTypeMeta
          alpha <- Contextual.freshTypeMeta
          _     <- Contextual.restore(Entry.TypeMetaVar(rho, Kind.Record), Entry.TypeMetaVar(alpha, Kind.Type))
          _     <- checkType(record, Type.Record(Vector(field -> alpha), Some(rho)))
        } yield alpha
    }

    action.flatMap(t => Contextual.get.map(_.apply(t))).traced(Activity.InferType(expression))
  }

  def inferApply(funType: Type, argumentExpression: Expression): Contextual[Type] =
    funType match {
      case Type.App(Type.App(Type.Arrow, alpha), beta) => checkType(argumentExpression, alpha).as(beta)
      case v @ Type.MVar(_) =>
        for {
          alpha <- Contextual.freshTypeMeta
          beta  <- Contextual.freshTypeMeta
          _     <- Contextual.restore(Entry.TypeMetaVar(alpha, Kind.Type), Entry.TypeMetaVar(beta, Kind.Type))
          _     <- unifies(v, Type.App(Type.App(Type.Arrow, alpha), beta))
          _     <- checkType(argumentExpression, alpha)
        } yield beta
      case Type.Forall(varName, kind, tpe) =>
        for {
          alpha <- Contextual.freshTypeMeta
          _     <- Contextual.restore(Entry.TypeMetaVar(alpha, kind))
          instantiated = tpe.substitute(Type.Var(varName), alpha)
          t <- inferApply(instantiated, argumentExpression)
        } yield t
      case _ => TypeError("Expected a function type").raiseError[Contextual, Type]
    }

  sealed trait ActualExpected[T, V <: T] {
    def metaVar: V
    def tpe: T
    def actual: T
    def expected: T
  }
  object ActualExpected {
    case class VarIsActual[T, V <: T](metaVar: V, tpe: T) extends ActualExpected[T, V] {
      override def actual: T   = metaVar
      override def expected: T = tpe
    }
    case class VarIsExpected[T, V <: T](metaVar: V, tpe: T) extends ActualExpected[T, V] {
      override def actual: T   = tpe
      override def expected: T = metaVar
    }
  }

  def unifies(actual: Type, expected: Type): Contextual[Unit] = {
    val action = (actual, expected) match {
      case (a @ Type.Record(_, _), e @ Type.Record(_, _)) => unifies(a, e)
      case (`expected`, _)                                => Contextual.unit
      case (_, Type.Record(Vector(), Some(`actual`)))     => Contextual.unit
      case (Type.Record(Vector(), Some(`expected`)), _)   => Contextual.unit
      case (_, Type.Forall(v, k, tpe)) =>
        Contextual.restore(Entry.Kinding(v, k)) >>
          unifies(actual, tpe) >>
          Contextual.stripFromVar(v)
      case (Type.Forall(v, k, tpe), _) =>
        for {
          alpha <- Contextual.freshTypeMeta
          instantiated = tpe.substitute(Type.Var(v), alpha)
          _ <- Contextual.restore(Entry.Marker, Entry.TypeMetaVar(alpha, k))
          _ <- unifies(instantiated, expected)
          _ <- Contextual.stripTail
        } yield ()
      case (Type.App(Type.App(Type.Arrow, a1), a2), Type.App(Type.App(Type.Arrow, e1), e2)) =>
        unifies(e1, a1) >> unifies(a2, e2)
      case (v @ Type.MVar(_), _) => solveType(ActualExpected.VarIsActual(v, expected))
      case (_, v @ Type.MVar(_)) => solveType(ActualExpected.VarIsExpected(v, actual))
      case _                     => TypeError("type mismatch").raiseError[Contextual, Unit]
    }
    action.traced(Activity.UnifyTypes(actual, expected))
  }

  def unifies(actual: Type.Record, expected: Type.Record): Contextual[Unit] = {
    val Type.Record(afields, atail) = actual
    val Type.Record(efields, etail) = expected
    val (aOnly, shared, eOnly)      = separate(afields, efields)
    for {
      _ <- shared.traverse_({ case (_, a, e) => unifies(a, e) })
      _ <-
        (atail, etail) match {
          case (None, None) =>
            if (eOnly.nonEmpty) TypeError("missing fields").raiseError[Contextual, Unit]
            else Contextual.unit
          case (Some(a), None) =>
            if (eOnly.isEmpty) Contextual.unit
            else
              for {
                rho <- Contextual.freshTypeMeta
                _   <- Contextual.restore(Entry.TypeMetaVar(rho, Kind.Record))
                _   <- unifies(a, Type.Record(eOnly, Some(rho)))
              } yield ()
          case (None, Some(e)) =>
            if (aOnly.isEmpty && eOnly.isEmpty) unifies(Type.Record(Vector.empty, None), e)
            else if (aOnly.nonEmpty && eOnly.isEmpty)
              for {
                rho <- Contextual.freshTypeMeta
                _   <- Contextual.restore(Entry.TypeMetaVar(rho, Kind.Record))
                _   <- unifies(Type.Record(aOnly, Some(rho)), e)
              } yield ()
            else TypeError("missing fields").raiseError[Contextual, Unit]
          case (Some(a), Some(e)) =>
            if (aOnly.isEmpty && eOnly.isEmpty) unifies(a, e)
            else
              for {
                rho <- Contextual.freshTypeMeta
                _   <- Contextual.restore(Entry.TypeMetaVar(rho, Kind.Record))
                _   <- unifies(Type.Record(aOnly, Some(rho)), e)
                _   <- unifies(a, Type.Record(eOnly, Some(rho)))
              } yield ()
        }
    } yield ()
  }

  def unifies(actual: Kind, expected: Kind): Contextual[Unit] = {
    val action = (actual, expected) match {
      case (`expected`, _)                          => Contextual.unit
      case (Kind.Record, Kind.Type)                 => Contextual.unit
      case (Kind.Arrow(a1, a2), Kind.Arrow(b1, b2)) => unifies(b1, a1) >> unifies(a2, b2)
      case (alpha @ Kind.MVar(_), _)                => solveKind(ActualExpected.VarIsActual(alpha, expected))
      case (_, alpha @ Kind.MVar(_))                => solveKind(ActualExpected.VarIsExpected(alpha, actual))
      case _                                        => TypeError("Kind mismatch").raiseError[Contextual, Unit]
    }

    action.traced(Activity.UnifyKinds(actual, expected))
  }

  def solveType(actualExpected: ActualExpected[Type, Type.MVar]): Contextual[Unit] = {
    for {
      ctx <- Contextual.get
      _ <- ctx.entries.head match {
        case e @ Entry.TypeMetaVar(v, k) =>
          if (v == actualExpected.metaVar) {
            val nextContext = ctx.copy(
              entries = Entry.TypeDefinition(v, k, actualExpected.tpe) :: ctx.suffix ++ ctx.entries.tail,
              suffix = Nil
            )
            Contextual.set(nextContext)
          } else if (v == actualExpected.tpe) {
            val nextContext = ctx.copy(
              entries = Entry.TypeDefinition(v, k, actualExpected.metaVar) :: ctx.suffix ++ ctx.entries.tail,
              suffix = Nil
            )
            Contextual.set(nextContext)
          } else if (actualExpected.tpe.free.contains(v)) {
            val nextContext = ctx.copy(entries = ctx.entries.tail, suffix = ctx.suffix :+ e)
            Contextual.set(nextContext) >> solveType(actualExpected)
          } else {
            Contextual.push >> solveType(actualExpected) >> Contextual.restore
          }
        case e @ Entry.TypeDefinition(v, _, t) =>
          val nextContext = ctx.copy(entries = ctx.entries.tail, stack = e :: ctx.stack)
          Contextual.set(nextContext) >>
            unifies(actualExpected.actual.substitute(v, t), actualExpected.expected.substitute(v, t)) >>
            Contextual.restore
        case Entry.Typing(name, t) =>
          if (t.free.contains(actualExpected.metaVar)) {
            val solved      = t.substitute(actualExpected.metaVar, actualExpected.tpe)
            val nextContext = ctx.copy(entries = ctx.entries.tail, stack = Entry.Typing(name, solved) :: ctx.stack)
            Contextual.set(nextContext) >>
              solveType(actualExpected) >>
              Contextual.restore
          } else {
            solveType(actualExpected) >>
              Contextual.restore
          }
        case _ => Contextual.push >> solveType(actualExpected) >> Contextual.restore
      }
    } yield ()
  }

  def solveKind(actualExpected: ActualExpected[Kind, Kind.MVar]): Contextual[Unit] = {
    for {
      ctx <- Contextual.get
      _ <- ctx.entries.head match {
        case e @ Entry.KindMetaVar(v) =>
          if (v == actualExpected.metaVar) {
            val nextContext = ctx.copy(
              entries = Entry.KindDefinition(v, actualExpected.tpe) :: ctx.suffix ++ ctx.entries.tail,
              suffix = Nil
            )
            Contextual.set(nextContext)
          } else if (v == actualExpected.tpe) {
            val nextContext = ctx.copy(
              entries = Entry.KindDefinition(v, actualExpected.metaVar) :: ctx.suffix ++ ctx.entries.tail,
              suffix = Nil
            )
            Contextual.set(nextContext)
          } else if (actualExpected.tpe.free.contains(v)) {
            val nextContext = ctx.copy(entries = ctx.entries.tail, suffix = ctx.suffix :+ e)
            Contextual.set(nextContext) >> solveKind(actualExpected)
          } else {
            Contextual.push >> solveKind(actualExpected) >> Contextual.restore
          }
        case e @ Entry.KindDefinition(v, k) =>
          val nextContext = ctx.copy(entries = ctx.entries.tail, stack = e :: ctx.stack)
          Contextual.set(nextContext) >>
            unifies(actualExpected.actual.substitute(v, k), actualExpected.expected.substitute(v, k)) >>
            Contextual.restore
        case Entry.Kinding(name, k) =>
          if (k.free.contains(actualExpected.metaVar)) {
            val solved      = k.substitute(actualExpected.metaVar, actualExpected.tpe)
            val nextContext = ctx.copy(entries = ctx.entries.tail, stack = Entry.Kinding(name, solved) :: ctx.stack)
            Contextual.set(nextContext) >>
              solveKind(actualExpected) >>
              Contextual.restore
          } else {
            solveKind(actualExpected) >>
              Contextual.restore
          }
        case Entry.TypeMetaVar(v, k) =>
          if (k.free.contains(actualExpected.metaVar)) {
            val solved      = k.substitute(actualExpected.metaVar, actualExpected.tpe)
            val nextContext = ctx.copy(entries = ctx.entries.tail, stack = Entry.TypeMetaVar(v, solved) :: ctx.stack)
            Contextual.set(nextContext) >>
              solveKind(actualExpected) >>
              Contextual.restore
          } else {
            solveKind(actualExpected) >>
              Contextual.restore
          }
        case _ => Contextual.push >> solveKind(actualExpected) >> Contextual.restore
      }
    } yield ()
  }

  def separate(
      afields: Vector[(String, Type)],
      bfields: Vector[(String, Type)]
  ): (Vector[(String, Type)], Vector[(String, Type, Type)], Vector[(String, Type)]) = {
    @tailrec
    def go(
        afields: List[(String, Type)],
        bfields: List[(String, Type)],
        acc: (Vector[(String, Type)], Vector[(String, Type, Type)], Vector[(String, Type)])
    ): (Vector[(String, Type)], Vector[(String, Type, Type)], Vector[(String, Type)]) =
      (afields, bfields, acc) match {
        case (Nil, Nil, _)                    => acc
        case (_, Nil, (aOnly, shared, bOnly)) => (aOnly ++ afields, shared, bOnly)
        case (Nil, _, (aOnly, shared, bOnly)) => (aOnly, shared, bOnly ++ bfields)
        case (ahd :: atl, bhd :: btl, (aOnly, shared, bOnly)) =>
          Order[String].comparison(ahd._1, bhd._1) match {
            case Comparison.GreaterThan => go(afields, btl, (aOnly, shared, bhd +: bOnly))
            case Comparison.EqualTo     => go(atl, btl, (aOnly, (ahd._1, ahd._2, bhd._2) +: shared, bOnly))
            case Comparison.LessThan    => go(atl, bfields, (ahd +: aOnly, shared, bOnly))
          }
      }

    go(afields.sortBy(_._1).toList, bfields.sortBy(_._1).toList, (Vector.empty, Vector.empty, Vector.empty))
  }
}
