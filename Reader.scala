package reader

import scala.util._
import actuator._



class Reader () {


  def head(raw: String): String = {
    val s = raw.trim
    var counter = 0
    if (s == "") ""
    else if (s.head == '(') s.takeWhile(c => {
      if (c == '(') counter = counter + 1
      if (c == ')') counter = counter - 1
      counter != 0
    }) + ")"
    else s.takeWhile(c => c != ' ' && c != '(')
  }

  def contentInParent(raw: String): String = {
    if (raw == "") ""
    else if (raw.head == '(' && raw.last == ')') raw.init.stripMargin('(').trim
    else raw
  }

  def isEmpty(raw: String): Boolean = contentInParent(raw.trim).trim == ""
  def isSingle(raw: String): Boolean = !contentInParent(raw.trim).trim.contains(' ')
  def isList(raw: String): Boolean = !isSingle(raw) && !isEmpty(raw)
  def isNumber(raw: String): Boolean = raw.trim.forall(c => c.isDigit)
  def isVar(raw: String): Boolean = isSingle(raw) && !isNumber(raw)

  def split(raw: String): (String, String) = {
    val s = raw.trim
    val ans = contentInParent(s).splitAt(head(contentInParent(s)).length)
    (ans._1.trim, ans._2.trim)
  }

  def read(raw: String): SExp = {

    if (raw == "") return Error("Nothing at all!")
    val s = raw.trim

    if (isSingle(s)) { val content = contentInParent(s)
      if (isNumber(content)) Number(content.toInt)
      else if (isVar(content)) {
        if (content == "let" ||
             content == "if" ||
              content == "'" ||
              content == "=" ||
              content == "+" ||
              content == "-" ||
              content == "*" ||
              content == "/") Op(content) else Var(content)
      }
      else Error("Weird single?")
    }
    else if (isList(s)) {
      val (fst, snd) = split(s)
      if (fst != "lambda") L(read(fst), read(snd))
      else read(snd) match {
        case L (Var(s), e) => Lambda(Var(s), e)
        case _ => Error("Wrong format of lambda!")
      }
    }
    else Error("Something wrong with input!")


  }// end read



}








