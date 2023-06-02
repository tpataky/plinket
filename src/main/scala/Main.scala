object Main {

  // fromInput: forall 'a . (forall 'i . { consortium: List {} 'i } -> 'a) -> Calculation 'a

  // if true then [] else [1]

  val examples: Vector[() => Unit] = Vector(
    { () =>
      val expr = parse("""fun a => a.b.c""")

      val context = Context.empty
      println(TypeCheck.inferType(expr).run(context, 0, 0))
      println(TypeCheck.checkType(expr, Type.fn(Type.Str, Type.Str)).run(context, 0, 0))
    },
    { () =>
      val expr = parse("""let id = fun a => a in id""")

      val context = Context.empty
      println(TypeCheck.inferType(expr).run(context, 0, 0))
      println(TypeCheck.checkType(expr, Type.fn(Type.Str, Type.Str)).run(context, 0, 0))
    },
    { () =>
      val expr = parse("""let id : forall a . a -> a = fun x => x in id""")

      val context = Context.empty
      println(TypeCheck.inferType(expr).run(context, 0, 0))
      println(TypeCheck.checkType(expr, Type.fn(Type.Str, Type.Str)).run(context, 0, 0))
    },
    { () =>
      val expr = parse("""fun a: {a: Num} => { a=1, b="s"}""")

      println(TypeCheck.checkType(expr, Type.fn(Type.Record(Vector("a" -> Type.Num, "d" -> Type.Str), None), Type.Record(Vector("a"->Type.Num), None))).run(Context.empty, 0, 0))
    },
    { () =>
      val expr = parse("""1""")

      println(TypeCheck.inferType(expr).run(Context.empty, 0, 0))
      println(TypeCheck.checkType(expr, Type.Num).run(Context.empty, 0, 0))
    },
    { () =>
      val expr = parse("""true""")

      println(TypeCheck.inferType(expr).run(Context.empty, 0, 0))
      println(TypeCheck.checkType(expr, Type.Bool).run(Context.empty, 0, 0))
    },
    { () =>
      val expr = parse("""true : Boolean""")

      println(TypeCheck.inferType(expr).run(Context.empty, 0, 0))
      println(TypeCheck.checkType(expr, Type.Bool).run(Context.empty, 0, 0))
    },
    { () =>
      val expr = parse("""a""")

      val context = Context(Entry.Typing("a", Type.Bool))
      println(TypeCheck.inferType(expr).run(context, 0, 0))
      println(TypeCheck.checkType(expr, Type.Bool).run(context, 0, 0))
    },
    { () =>
      val expr = parse("""a b""")

      val context = Context(Entry.Typing("a", Type.fn(Type.Bool, Type.Str)), Entry.Typing("b", Type.Bool))
      println(TypeCheck.inferType(expr).run(context, 0, 0))
      println(TypeCheck.checkType(expr, Type.Str).run(context, 0, 0))
    },
    { () =>
      val expr = parse("""fun a => a""")

      val context = Context.empty
      println(TypeCheck.inferType(expr).run(context, 0, 0))
      println(TypeCheck.checkType(expr, Type.fn(Type.Str, Type.Str)).run(context, 0, 0))
    },
    { () =>
      val expr = parse("""fun a => a.b""")

      val context = Context.empty
      println(TypeCheck.inferType(expr).run(context, 0, 0))
      println(TypeCheck.checkType(expr, Type.fn(Type.Str, Type.Str)).run(context, 0, 0))
    },
    { () =>
      val expr = parse("""fun a => plus a.b a.c""")

      val context = Context(Entry.Typing("plus", Type.fn(Type.Num, Type.fn(Type.Num, Type.Num))))
      println(TypeCheck.inferType(expr).run(context, 0, 0))
      println(TypeCheck.checkType(expr, Type.fn(Type.Str, Type.Str)).run(context, 0, 0))
    },
    { () =>
      val expr = parse("""fun a: {b: Num, c: Num, d: String} => plus a.b a.c""")

      val context = Context(Entry.Typing("plus", Type.fn(Type.Num, Type.fn(Type.Num, Type.Num))))
      println(TypeCheck.inferType(expr).run(context, 0, 0))
      println(TypeCheck.checkType(expr, Type.fn(Type.Str, Type.Str)).run(context, 0, 0))
    }
  )

  def main(args: Array[String]): Unit = {

    examples(0)()
  }

  def parse(expr: String): Expression =
    Parser.expression.parseAll(expr) match {
      case Left(value)  => sys.error(value.toString)
      case Right(value) => value
    }
}
