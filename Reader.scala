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
    else s.takeWhile(c => c != ' ' && c != '(' && c != '\n')
  }

  def contentInParent(raw: String): String = {
    if (raw == "") ""
    else if (raw.head == '(' && raw.last == ')') raw.init.map(
      c => if (c == '\n') ' ' else c).stripMargin('(').trim
    else raw
  }

  def checkParents(raw: String): Boolean = {
    def check(s: String, res: Int): Boolean = {
      if (s == "") (if (res == 0) true else false)
      else if (s.head == '(') check(s.tail, res + 1)
      else if (s.head == ')') check(s.tail, res - 1)
      else check(s.tail, res)
    }
    check(raw.trim, 0)
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
    if (raw == "") return Error("Nothing at all!", Symbol("Empty input!"))
    if (checkParents(raw) == false)
      return Error("Parentheses not match! please check!", Symbol(raw.trim))
    val s = raw.trim.lines.filter(l =>
      if (l.trim != "") l.trim.head != ';' else false).foldLeft("")(_ + _ + "\n").trim

    process(s)

  }

  def process(raw: String): SExp = {

    val s = raw.trim

    if (isSingle(s)) { val content = contentInParent(s)
      if (isNumber(content)) Number(content.toInt)
      else if (isVar(content)) {
        if ( content == ":=" ||
             content == "if" ||
              content == "'" ||
              content == "=" ||
              content == "+" ||
              content == "-" ||
              content == "*" ||
              content == "/") Op(content) else Var(content)
      }
      else Error("Weird single?", Symbol(s))
    }
    else if (isList(s)) {
      val (fst, snd) = split(s)
      if (fst != "lambda") L(process(fst), process(snd))
      else read(snd) match {
        case L (Var(s), e) => Lambda(Var(s), e)
        case _ => Error("Wrong format of lambda!", process(snd))
      }
    }
    else Error("Something wrong with input!", Symbol("sth wrong"))


  }// end read



}








