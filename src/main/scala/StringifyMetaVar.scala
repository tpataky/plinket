object StringifyMetaVar {

  def apply(i: Int): String = {
    def go(i: Int): String =
      if (i >= letters.length) go(i / letters.length - 1) + letters(i % letters.length).toChar
      else letters(i).toChar.toString

    "'" ++ go(i)
  }

  private val letters = Range.inclusive('A', 'Z').toArray

}
