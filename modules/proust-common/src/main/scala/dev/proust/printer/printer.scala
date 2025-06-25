package dev.proust.printer

extension (str: String)
  def inParens: String =
    s"($str)"

  def inParensIf(condition: Boolean): String =
    if condition then str.inParens else str
