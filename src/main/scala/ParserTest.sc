import Parser._
import cats.parse.{Parser => P}

val p = (((P.string("let ") *> ws *> variable <* ws) ~ (P.char(':') *> ws *> typeExpression).?) ~
  (ws *> P.char('=') *> ws *> P.defer(expression)))

expression.parse("""let f = (fun a => a) in id""")
