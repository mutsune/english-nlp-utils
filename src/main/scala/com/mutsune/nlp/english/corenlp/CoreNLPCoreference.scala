package com.mutsune.nlp.english.corenlp

import scala.collection.mutable

/**
  * Created by nakayama.
  */
class CoreNLPCoreference(entities: Seq[Seq[CoreNLPToken]])

object CoreNLPCoreference {

    val coreNLPCorefMentionMap = mutable.Map.empty[Seq[Seq[CoreNLPToken]], CoreNLPCoreference]

    def apply(entities: Seq[Seq[CoreNLPToken]]): CoreNLPCoreference =
        coreNLPCorefMentionMap.getOrElseUpdate(entities, new CoreNLPCoreference(entities))

}
