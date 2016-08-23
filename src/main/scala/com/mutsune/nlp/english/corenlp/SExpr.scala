package com.mutsune.nlp.english.corenlp

import scala.util.parsing.combinator.RegexParsers

/**
  * Created by nakayama.
  */
object SExpr extends RegexParsers {

    def apply(str: String): Option[NonTerminal] = SExpr.parseAll(s, str) match {
        case Success(root, _) => {
            def setParent(node: Node): Unit = node match {
                case n: NonTerminal => n.children.foreach { c =>
                    c.parent = Some(n)
                    setParent(c)
                }
                case n: Terminal =>
            }
            setParent(root)

            Some(root)
        }
        case _ => None
    }

    def s: Parser[NonTerminal] = nonTerminal

    def node: Parser[Node] = terminal | nonTerminal

    def nonTerminal: Parser[NonTerminal] = "(" ~> label ~ rep(node) <~ ")" map {
        case label ~ children => new NonTerminal(label, children)
    }

    def terminal: Parser[Terminal] = "(" ~> label ~ word <~ ")" map {
        case label ~ word => new Terminal(label, word)
    }

    def label: Parser[String] = """[^\(\) ]+""".r

    def word: Parser[String] = """[^\(\) ]+""".r

}

abstract case class Node(label: String) {

    def text: String

    // TODO: change to getter/setter
    var parent: Option[NonTerminal]

}

class NonTerminal(label: String, val children: List[Node]) extends Node(label) {

    def text = children.map(_.text).mkString(" ")

    private[this] def getDescendantTerminals(node: Node): List[Terminal] = node match {
        case n: NonTerminal => n.children.flatMap(getDescendantTerminals)
        case n: Terminal => n :: Nil
    }

    lazy val descendantTerminals: List[Terminal] = getDescendantTerminals(this)

    override var parent: Option[NonTerminal] = None

}

class Terminal(label: String, val word: String) extends Node(label) {

    def text = word

    override var parent: Option[NonTerminal] = None

}
