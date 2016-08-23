package com.mutsune.nlp.english.corenlp

import java.io.{File, PrintStream}
import java.util.Properties

import com.mutsune.util.io.File.FileOps
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.xml.{NodeSeq, XML}

/**
  * Created by nakayama.
  */
final class CoreNLP(text: String) {

    private[this] val properties: Properties = new Properties()
    properties.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse, depparse, dcoref")
    //    properties.put("tokenize.options",
    //        Seq("tokenizeNLs=false",
    //            "invertible=false",
    //            "ptb3Escaping=false",
    //            "americanize=false",
    //            "normalizeAmpersandEntity=false",
    //            "normalizeSpace=false",
    //            "normalizeParentheses=false",
    //            "normalizeOtherBrackets=false",
    //            "asciiQuotes=false",
    //            "latexQuotes=false",
    //            "unicodeQuotes=false").mkString(","))
    //    properties.put("dcoref.score", true)
    //    properties.put("dcoref.maxdist", -1)


    private[this] val coreNLP: StanfordCoreNLP = new StanfordCoreNLP(properties)
    private[this] val annotation: Annotation = new Annotation(text)
    coreNLP.annotate(annotation)

    val sentences: Array[CoreNLPSentence] = {
        val sentences: Array[CoreNLPSentence] = annotation.get(classOf[SentencesAnnotation]).map(new CoreNLPSentence(_)).toArray

        // Coreference annotation
        for (corefChains <- annotation.get(classOf[CorefChainAnnotation]).values()) {
            val corefChain: Seq[Seq[CoreNLPToken]] = corefChains.getMentionMap.values().map { chain =>
                // Be able to get only one mention
                chain.map(m => sentences(m.sentNum - 1).tokens.slice(m.startIndex - 1, m.endIndex - 1)).toSeq
            }.toIndexedSeq.flatten
            // TODO: register a representative
            corefChain.flatten.foreach(_.coreference = corefChain)
        }

        sentences
    }

    def writeAsXML(file: File): Unit = writeAsXML(new PrintStream(FileOps.withMakeParentDirs(file)))

    def writeAsXML(writer: PrintStream): Unit = coreNLP.xmlPrint(annotation, writer)

}

object CoreNLP {

    type CoreNLPDoc = Seq[CoreNLPSentence]

    final private[this] val coreNLPs: mutable.Map[String, CoreNLP] = mutable.Map.empty[String, CoreNLP]

    val coreNLPDocCache: mutable.Map[String, CoreNLPDoc] = mutable.Map.empty[String, CoreNLPDoc]

    def apply(text: String): CoreNLPDoc = {
        coreNLPDocCache.getOrElseUpdate(text, {
            val coreNLP = new CoreNLP(text)
            coreNLPs += text -> coreNLP
            coreNLP.sentences
        })
    }

    def loadXML(xmlFile: File): CoreNLPDoc = {
        println("loading Stanford CoreNLP XML file...: " + xmlFile.getName)
        loadXML(XML.loadFile(xmlFile))
    }

    def loadXML(root: NodeSeq): CoreNLPDoc = {
        def getTokenInfoFromNode(parentNode: NodeSeq, nodeName: String): Option[String] =
            parentNode \\ nodeName match {
                case n if n.nonEmpty => Some(n.text)
                case _ => None
            }

        val sentenceNodes: NodeSeq = root \ "document" \ "sentences" \ "sentence"
        val coreNLPDoc: CoreNLPDoc = sentenceNodes.map { sentNode =>
            val parseTree: Option[Node] = getTokenInfoFromNode(sentNode, "parse") match {
                case Some(sExpr) => SExpr(sExpr)
                case _ => None
            }
            val parseNodes: Array[Terminal] = parseTree match {
                case s: Some[NonTerminal] => s.get.descendantTerminals.toArray
                case _ => Array()
            }
            val basicDepParseTree: Option[String] =
                (sentNode \\ "dependencies").find(n => (n \\ "@type").text == "basic-dependencies") match {
                    case Some(n) if n.nonEmpty => Some(n.text)
                    case _ => None
                }

            val tokens: Seq[CoreNLPToken] = (sentNode \\ "token").zipWithIndex.map { case (tknNode, i) =>
                val surface: Option[String] = (tknNode \\ "word").text match {
                    case "``" => Some("\"")
                    case "''" => Some("\"")
                    case "`" => Some("'")
                    case "-LRB-" => Some("(")
                    case "-RRB-" => Some(")")
                    case "" => None
                    case s => Some(s)
                }
                val lemma: Option[String] = getTokenInfoFromNode(tknNode, "lemma")
                val startOffset: Option[Int] = try {
                    getTokenInfoFromNode(tknNode, "CharacterOffsetBegin").map(_.toInt)
                } catch {
                    case e: java.lang.NumberFormatException => {
                        System.err.println(
                            "java.lang.NumberFormatException: For input string: " + getTokenInfoFromNode(tknNode, "CharacterOffsetBegin"))
                        throw new java.lang.NumberFormatException
                    }
                }
                val endOffset: Option[Int] = getTokenInfoFromNode(tknNode, "CharacterOffsetEnd").map(_.toInt)
                val pos: Option[String] = getTokenInfoFromNode(tknNode, "POS")
                val ne: Option[String] = getTokenInfoFromNode(tknNode, "NER")

                val node: Option[Node] = if (i < parseNodes.length) Some(parseNodes(i)) else None

                new CoreNLPToken(startOffset, endOffset, surface, lemma, pos, ne, node)
            }
            new CoreNLPSentence(tokens, parseTree, basicDepParseTree)
        }

        // Coreference annotation
        for {
            corefChainNode <- (root \ "document" \ "coreference" \ "coreference")
        } {
            val corefChain: Seq[Seq[CoreNLPToken]] = (corefChainNode \ "mention").map { m =>
                val sentenceIndex: Int = (m \ "sentence").text.toInt
                val startIndex: Int = (m \ "start").text.toInt
                val endIndex: Int = (m \ "end").text.toInt
                val headIndex: Int = (m \ "head").text.toInt
                val text: String = (m \ "text").text
                coreNLPDoc(sentenceIndex - 1).tokens.slice(startIndex - 1, endIndex - 1)
            }
            corefChain.flatten.foreach(_.coreference = corefChain)
        }

        // Append to cache
        coreNLPDocCache += coreNLPDoc.map(_.text).mkString(" ") -> coreNLPDoc

        coreNLPDoc
    }

    def writeAsXML(path: String): Unit =
        coreNLPs.foreach { case (t, c) => c.writeAsXML(new File(s"${path}/${t.hashCode}")) }

}
