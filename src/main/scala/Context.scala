import cats.MonadError
import cats.syntax.either._
import io.github.tpataky.duckling.{Doc, LayoutOpts, Renderer}

import scala.annotation.tailrec
import scala.io.AnsiColor

case class TypeError(msg: String) extends RuntimeException

sealed trait Entry {

  override def toString: String = toDoc.render(Contextual.opts, Contextual.renderer)

  def toDoc: Doc[Unit] =
    this match {
      case Entry.Typing(name, tpe)            => Doc(name + ":") + tpe.toDoc
      case Entry.Kinding(name, kind)          => Doc(name + ":") + kind.toDoc
      case Entry.TypeMetaVar(v, kind)         => v.toDoc + Doc.colon + kind.toDoc
      case Entry.TypeDefinition(v, kind, tpe) => v.toDoc + Doc.colon + kind.toDoc + Doc(":=") + tpe.toDoc
      case Entry.KindMetaVar(v)               => v.toDoc
      case Entry.KindDefinition(v, kind)      => v.toDoc + Doc(":=") + kind.toDoc
      case Entry.Marker                       => Doc.semi
    }
}

object Entry {
  case class Typing(name: String, tpe: Type)                     extends Entry
  case class Kinding(name: String, kind: Kind)                   extends Entry
  case class TypeMetaVar(v: Type.MVar, kind: Kind)               extends Entry
  case class TypeDefinition(v: Type.MVar, kind: Kind, tpe: Type) extends Entry
  case class KindMetaVar(v: Kind.MVar)                           extends Entry
  case class KindDefinition(v: Kind.MVar, kind: Kind)            extends Entry
  case object Marker                                             extends Entry
}

case class Context(entries: List[Entry], suffix: List[Entry], stack: List[Entry]) {
  def apply(k: Kind): Kind =
    entries.foldLeft(k)({
      case (k, Entry.KindDefinition(v, k1)) => k.substitute(v, k1)
      case (k, _)                           => k
    })

  def apply(t: Type): Type =
    entries.foldLeft(t)({
      case (t, Entry.TypeDefinition(v, _, t1)) => t.substitute(v, t1)
      case (t, _)                              => t
    })
}
object Context {
  val empty: Context = Context(Nil, Nil, Nil)

  def apply(entries: Entry*): Context = Context(entries.reverse.toList, Nil, Nil)
}

sealed trait Activity
object Activity {
  case class UnifyKinds(actual: Kind, expected: Kind)                  extends Activity
  case class UnifyTypes(actual: Type, expected: Type)                  extends Activity
  case class CheckType(expression: Expression, expected: Type)         extends Activity
  case class InferType(expression: Expression)                         extends Activity
  case class CheckKind(typeExpression: TypeExpression, expected: Kind) extends Activity
  case class InferKind(typeExpression: TypeExpression)                 extends Activity
}

trait Contextual[+A] { self =>
  def run(ctx: Context, nextId: Int, depth: Int): Either[TypeError, A] =
    Either.catchOnly[TypeError](doRun(ctx, nextId, depth)._3)

  def doRun(ctx: Context, nextId: Int, depth: Int): (Context, Int, A)

  def traced(activity: Activity): Contextual[A] = new Contextual[A] {
    import Contextual._

    override def doRun(ctx: Context, nextId: Int, depth: Int): (Context, Int, A) = {
      val (colour, text) = activity match {
        case Activity.UnifyKinds(actual, expected) =>
          AnsiColor.CYAN -> (actual.toDoc + Doc("===") + expected.toDoc).render(opts, renderer)
        case Activity.UnifyTypes(actual, expected) =>
          AnsiColor.BOLD -> (actual.toDoc + Doc("===") + expected.toDoc).render(opts, renderer)
        case Activity.CheckKind(typeExpression, expected) =>
          AnsiColor.GREEN -> (typeExpression.toDoc + Doc("<==") + expected.toDoc)
            .render(opts, renderer)
        case Activity.InferKind(typeExpression) =>
          (AnsiColor.BOLD + AnsiColor.GREEN) -> (typeExpression.toDoc + Doc("==>"))
            .render(opts, renderer)
        case Activity.CheckType(expression, expected) =>
          AnsiColor.BLUE -> (expression.toDoc + Doc("<==") + expected.toDoc)
            .render(opts, renderer)
        case Activity.InferType(expression) =>
          (AnsiColor.BOLD + AnsiColor.BLUE) -> (expression.toDoc + Doc("==>"))
            .render(opts, renderer)
      }

      val firstIndent = ("| " * (depth - 1)) + ("|-" * (Math.min(1, depth)))
      val indent      = "| " * depth
      println(firstIndent ++ colour ++ text ++ AnsiColor.RESET)
      println(indent ++ colour ++ s"  [${ctx.entries.reverse.mkString(",")}|${ctx.suffix.mkString(",")}|${ctx.stack
          .mkString(",")}]" ++ AnsiColor.RESET)

      try {
        val out = self.doRun(ctx, nextId, depth + 1)

        println(indent ++ colour ++ s"  [${out._1.entries.reverse.mkString(",")}|${ctx.suffix.mkString(",")}|${ctx.stack
            .mkString(",")}]" ++ AnsiColor.RESET)

        activity match {
          case Activity.InferType(_) =>
            println(
              indent ++ colour ++ text ++ out
                .asInstanceOf[(Context, Int, Type)]
                ._3
                .toDoc
                .render(opts, renderer) ++ AnsiColor.RESET
            )
          case Activity.InferKind(_) =>
            println(
              indent ++ colour ++ text ++ out
                .asInstanceOf[(Context, Int, (Type, Kind))]
                ._3
                ._2
                .toDoc
                .render(opts, renderer) ++ AnsiColor.RESET
            )
          case _ =>
            println(indent ++ colour ++ text ++ " ok" ++ AnsiColor.RESET)
        }

        out
      } catch {
        case e: TypeError =>
          println(indent ++ colour ++ s"  [${ctx.entries.reverse.mkString(",")}|${ctx.suffix.mkString(",")}|${ctx.stack
              .mkString(",")}]" ++ AnsiColor.RESET)
          println(indent ++ colour ++ "failed " ++ text ++ AnsiColor.RESET)
          throw e
      }
    }
  }
}

object Contextual {
  val opts     = LayoutOpts(240, 2)
  val renderer = Renderer.string(System.lineSeparator())

  implicit val monadError: MonadError[Contextual, TypeError] = new MonadError[Contextual, TypeError] {
    override def raiseError[A](e: TypeError): Contextual[A] = new Contextual[A] {
      override def doRun(ctx: Context, nextId: Int, depth: Int): (Context, Int, A) = throw e
    }

    override def handleErrorWith[A](fa: Contextual[A])(f: TypeError => Contextual[A]): Contextual[A] =
      new Contextual[A] {
        override def doRun(ctx: Context, nextId: Int, depth: Int): (Context, Int, A) = {
          try {
            fa.doRun(ctx, nextId, depth)
          } catch {
            case e: TypeError => f(e).doRun(ctx, nextId, depth)
          }
        }
      }

    override def pure[A](x: A): Contextual[A] = new Contextual[A] {
      override def doRun(ctx: Context, nextId: Int, depth: Int): (Context, Int, A) = (ctx, nextId, x)
    }

    override def flatMap[A, B](fa: Contextual[A])(f: A => Contextual[B]): Contextual[B] = new Contextual[B] {
      override def doRun(ctx: Context, nextId: Int, depth: Int): (Context, Int, B) = {
        val (nextContext, nextNextId, a) = fa.doRun(ctx, nextId, depth)
        f(a).doRun(nextContext, nextNextId, depth)
      }
    }

    override def tailRecM[A, B](a: A)(f: A => Contextual[Either[A, B]]): Contextual[B] = new Contextual[B] {
      override def doRun(ctx: Context, nextId: Int, depth: Int): (Context, Int, B) = loop(a, ctx, nextId, depth)

      @tailrec
      def loop(a: A, ctx: Context, nextId: Int, depth: Int): (Context, Int, B) = {
        f(a).doRun(ctx, nextId, depth) match {
          case (nextContext, nextNextId, Left(a))  => loop(a, nextContext, nextNextId, depth)
          case (nextContext, nextNextId, Right(b)) => (nextContext, nextNextId, b)
        }
      }
    }
  }

  val unit: Contextual[Unit] = new Contextual[Unit] {
    override def doRun(ctx: Context, nextId: Int, depth: Int): (Context, Int, Unit) = (ctx, nextId, ())
  }

  val get: Contextual[Context] = new Contextual[Context] {
    override def doRun(ctx: Context, nextId: Int, depth: Int): (Context, Int, Context) = (ctx, nextId, ctx)
  }

  def set(ctx: Context): Contextual[Unit] = new Contextual[Unit] {
    override def doRun(ctx0: Context, nextId: Int, depth: Int): (Context, Int, Unit) = (ctx, nextId, ())
  }

  def modify(f: Context => Context): Contextual[Unit] = new Contextual[Unit] {
    override def doRun(ctx0: Context, nextId: Int, depth: Int): (Context, Int, Unit) = {
      val nextContext = f(ctx0)
      (nextContext, nextId, ())
    }
  }

  val push: Contextual[Unit] =
    modify(ctx => ctx.copy(entries = ctx.entries.tail, stack = ctx.entries.head :: ctx.stack.tail))

  val restore: Contextual[Unit] =
    modify(ctx => ctx.copy(entries = ctx.stack.head :: ctx.entries, stack = ctx.stack.tail))

  def restore(entries: Entry*): Contextual[Unit] =
    modify(ctx => ctx.copy(entries = ctx.entries.prependedAll(entries.reverse)))

  def lookupTypeVar(name: String): Contextual[Option[Kind]] = new Contextual[Option[Kind]] {
    override def doRun(ctx: Context, nextId: Int, depth: Int): (Context, Int, Option[Kind]) =
      (ctx, nextId, ctx.entries.collectFirst({ case Entry.Kinding(`name`, k) => k }))
  }

  def lookupVar(name: String): Contextual[Option[Type]] = new Contextual[Option[Type]] {
    override def doRun(ctx: Context, nextId: Int, depth: Int): (Context, Int, Option[Type]) =
      (ctx, nextId, ctx.entries.collectFirst({ case Entry.Typing(`name`, t) => t }))
  }

  val stripTail: Contextual[List[Entry.TypeMetaVar]] = new Contextual[List[Entry.TypeMetaVar]] {
    override def doRun(ctx: Context, nextId: Int, depth: Int): (Context, Int, List[Entry.TypeMetaVar]) = {
      def loop(entries: List[Entry], acc: List[Entry.TypeMetaVar]): (List[Entry], List[Entry.TypeMetaVar]) =
        entries match {
          case (e @ Entry.TypeMetaVar(_, _)) :: tl => loop(tl, e :: acc)
          case Entry.Marker :: tl                  => (tl, acc)
          case _ :: tl                             => loop(tl, acc)
          case Nil                                 => (Nil, acc)
        }

      val (remainingEntries, unsolved) = loop(ctx.entries, Nil)
      (ctx.copy(entries = remainingEntries), nextId, unsolved)
    }
  }

  def stripFromVar(v: String): Contextual[Unit] =
    modify({ ctx =>
      val entries = ctx.entries
        .dropWhile({
          case Entry.Kinding(`v`, _) | Entry.Typing(`v`, _) => false
          case _                                            => true
        })
        .drop(1)
      ctx.copy(entries = entries)
    })

  val freshTypeMeta: Contextual[Type.MVar] = new Contextual[Type.MVar] {
    override def doRun(ctx: Context, nextId: Int, depth: Int): (Context, Int, Type.MVar) =
      (ctx, nextId + 1, Type.MVar(nextId))
  }

  val freshKindMeta: Contextual[Kind.MVar] = new Contextual[Kind.MVar] {
    override def doRun(ctx: Context, nextId: Int, depth: Int): (Context, Int, Kind.MVar) =
      (ctx, nextId + 1, Kind.MVar(nextId))
  }
}
