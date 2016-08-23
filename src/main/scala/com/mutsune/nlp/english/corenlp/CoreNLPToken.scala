package com.mutsune.nlp.english.corenlp

import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.util.CoreMap

/**
  * Created by nakayama.
  */
case class CoreNLPToken(coreMap: CoreMap, coreLabel: CoreLabel) {

    def this(startOffset: Option[Int],
             endOffset: Option[Int],
             surface: Option[String],
             lemma: Option[String],
             pos: Option[String],
             ne: Option[String],
             node: Option[Node]) = {
        this(null, null)
        this.startOffset = startOffset
        this.endOffset = endOffset
        this.surface = surface
        this.lemma = lemma
        this.pos = pos
        this.ne = ne
        this.node = node
    }

    private[this] var _startOffset: Option[Int] = {
        if (coreLabel != null) {
            Some(coreLabel.get(classOf[CharacterOffsetBeginAnnotation]))
        } else None
    }

    def startOffset: Option[Int] = _startOffset

    private def startOffset_=(startOffset: Option[Int]) = _startOffset = startOffset

    private[this] var _endOffset: Option[Int] = {
        if (coreLabel != null) {
            Some(coreLabel.get(classOf[CharacterOffsetEndAnnotation]))
        } else None
    }

    def endOffset: Option[Int] = _endOffset

    private[this] def endOffset_=(endOffset: Option[Int]) = _endOffset = endOffset

    private[this] var _surface: Option[String] = {
        if (coreLabel != null) {
            coreLabel.get(classOf[OriginalTextAnnotation]) match {
                case null => None
                case s => Some(s)
            }
        } else None
    }

    def surface: Option[String] = _surface

    private[this] def surface_=(surface: Option[String]) = _surface = surface

    private[this] var _lemma: Option[String] = {
        if (coreLabel != null) {
            coreLabel.get(classOf[LemmaAnnotation]) match {
                case null => None
                case s => Some(s)
            }
        } else None
    }

    def lemma: Option[String] = _lemma

    private[this] def lemma_=(lemma: Option[String]) = _lemma = lemma

    private[this] var _pos: Option[String] = {
        if (coreLabel != null) {
            coreLabel.get(classOf[PartOfSpeechAnnotation]) match {
                case null => None
                case p => Some(p)
            }
        } else None
    }

    def pos: Option[String] = _pos

    private[this] def pos_=(pos: Option[String]) = _pos = pos

    private[this] var _ne: Option[String] = {
        if (coreLabel != null) {
            coreLabel.get(classOf[NamedEntityTagAnnotation]) match {
                case null => None
                case n => Some(n)
            }
        } else None
    }

    def ne: Option[String] = _ne

    private[this] def ne_=(ne: Option[String]) = _ne = ne

    private[this] var _node: Option[Node] = None

    def node: Option[Node] = _node

    def node_=(node: Option[Node]) = _node = node

    private[nlp] var _coreference: Option[CoreNLPCoreference] = None

    def coreference: Option[CoreNLPCoreference] = _coreference

    def coreference_=(corefTokens: Seq[Seq[CoreNLPToken]]) = _coreference = Some(CoreNLPCoreference(corefTokens))

    override def toString: String = (
        startOffset getOrElse None,
        endOffset getOrElse None,
        surface getOrElse None,
        lemma getOrElse None,
        pos getOrElse None,
        ne getOrElse None,
        node getOrElse None,
        coreference getOrElse None
        ).toString

}
