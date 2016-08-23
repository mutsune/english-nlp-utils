package com.mutsune.nlp.english.wordnet

import edu.cmu.lti.lexical_db.NictWordNet
import edu.cmu.lti.ws4j.impl._
import edu.cmu.lti.ws4j.util.WS4JConfiguration

/**
  * Created by nakayama.
  */
object WordNet {

    private[this] val db = new NictWordNet()

    WS4JConfiguration.getInstance().setMFS(true)

    private[this] val hirstStOnge: HirstStOnge = new HirstStOnge(db)
    private[this] val leacockChodorow: LeacockChodorow = new LeacockChodorow(db)
    private[this] val lesk: Lesk = new Lesk(db)
    private[this] val wuPalmer: WuPalmer = new WuPalmer(db)
    private[this] val resnik: Resnik = new Resnik(db)
    private[this] val jiangConrath: JiangConrath = new JiangConrath(db)
    private[this] val lin: Lin = new Lin(db)
    private[this] val path: Path = new Path(db)

    def hirstStOngeSimilarity(word1: String, word2: String) = hirstStOnge.calcRelatednessOfWords(word1, word2)

    def leacockChodorowSimilarity(word1: String, word2: String) = leacockChodorow.calcRelatednessOfWords(word1, word2)

    def leskSimilarity(word1: String, word2: String) = lesk.calcRelatednessOfWords(word1, word2)

    def wuPalmerSimilarity(word1: String, word2: String) = wuPalmer.calcRelatednessOfWords(word1, word2)

    def resnikSimilarity(word1: String, word2: String) = resnik.calcRelatednessOfWords(word1, word2)

    def jiangConrathSimilarity(word1: String, word2: String) = jiangConrath.calcRelatednessOfWords(word1, word2)

    def linSimilarity(word1: String, word2: String) = lin.calcRelatednessOfWords(word1, word2)

    def pathSimilarity(word1: String, word2: String) = path.calcRelatednessOfWords(word1, word2)

}
