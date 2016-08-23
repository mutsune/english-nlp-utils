package com.mutsune.nlp.english.corenlp

import edu.stanford.nlp.ling.CoreAnnotations.TokensAnnotation
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations.BasicDependenciesAnnotation
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation
import edu.stanford.nlp.util.CoreMap

import scala.collection.JavaConversions._

/**
  * Created by nakayama.
  */
case class CoreNLPSentence(coreMap: CoreMap) {

    def this(tokens: Seq[CoreNLPToken],
             // TODO: String to Tree and SemanticGraph
             parseTree: Option[Node],
             basicDepParseTree: Option[String]) = {
        this(null)
        this.tokens = tokens
        this.parseTree = parseTree
        this.basicDepParseTree = basicDepParseTree
    }

    private[this] var _tokens: Seq[CoreNLPToken] = coreMap match {
        case c if c != null => c.get(classOf[TokensAnnotation]).map(CoreNLPToken(coreMap, _))
        case _ => Nil
    }

    def tokens: Seq[CoreNLPToken] = _tokens

    private[this] def tokens_=(tokens: Seq[CoreNLPToken]) = _tokens = tokens

    lazy val text: String = {
        // Build a sentence text
        val text = new StringBuilder
        val firstSentenceOffset: Int = tokens.headOption match {
            case Some(t) if t.startOffset.isDefined => t.startOffset.get
            case _ => 0
        }
        tokens.foreach { t =>
            // Fill spaces
            val startOffset: Int = t.startOffset match {
                case Some(o) => o
                case _ => text.length
            }
            val spaces: String = Range(0, startOffset - firstSentenceOffset - text.length).map(_ => ' ').mkString
            text.append(spaces + t.surface.getOrElse(""))
        }
        text.result
    }

    private[this] var _parseTree: Option[Node] = {
        if (coreMap != null) {
            coreMap.get(classOf[TreeAnnotation]) match {
                case null => None
                case t => SExpr(t.toString)
            }
        } else None
    }

    def parseTree: Option[Node] = _parseTree

    private[this] def parseTree_=(parseTree: Option[Node]) = _parseTree = parseTree

    private[this] var _basicDepParseTree: Option[String] = {
        if (coreMap != null) {
            coreMap.get(classOf[BasicDependenciesAnnotation]) match {
                case null => None
                case t => Some(t.toString)
            }
        } else None
    }

    def basicDepParseTree: Option[String] = _basicDepParseTree

    private[this] def basicDepParseTree_=(basicDepParseTree: Option[String]) = _basicDepParseTree = basicDepParseTree

    (parseTree match {
        case someRoot: Some[NonTerminal] => someRoot.get.descendantTerminals
        case _ => Nil
    }) zip tokens foreach { case (n, t) =>
        t.node = Some(n)
    }

}
