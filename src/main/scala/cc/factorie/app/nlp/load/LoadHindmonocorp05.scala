package cc.factorie.app.nlp.load

import cc.factorie.app.nlp.pos.{LabeledHindiPosTag, LabeledGermanPosTag, PosTag}
import cc.factorie.app.nlp.{Document, Sentence, Token, UnknownDocumentAnnotator, pos, transliterate}

/**
 * Created by Esther on 6/18/15.
 */
object LoadHindmonocorp05 extends Load {

  def fromSource(source:io.Source): Seq[Document] = {
    import scala.collection.mutable.ArrayBuffer
    def newDocument(name: String): Document = {
      val document = new Document("").setName(name)
      document.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass // register that we have token boundaries
      document.annotators(classOf[Sentence]) = UnknownDocumentAnnotator.getClass // register that we have sentence boundaries
      document.annotators(classOf[pos.GermanPosTag]) = UnknownDocumentAnnotator.getClass // register that we have POS tags
      document
    }

    val documents = new ArrayBuffer[Document]
    var document = newDocument("Hindmonocorp" + documents.length)
    documents += document
    var sentence = new Sentence(document)
    for (line <- source.getLines()) {
      if (line.length < 2) {
        document.appendString("\n")
        sentence = new Sentence(document)
      }
      else {
        val fields = line.split('\t')
        //assert(fields.length == 15)
        //print(fields(0) + "\t")
        val word = cc.factorie.app.nlp.transliterate.transliterateHindi(fields(0))
        //println(word)
        val partOfSpeech = fields(2)
        if (sentence.length > 0) document.appendString(" ")
        val token = new Token(sentence, word)
        token.attr += new LabeledHindiPosTag(token, partOfSpeech)
      }
    }
    documents
  }

  def main(args: Array[String]) = {
    // here we assume first arg is just the data filename
    val fname = args(0)
    val docs = fromFilename(fname)


    // just print out the first document loaded, one word per line
    //docs.head.tokens.foreach{token => println(s"${token.string}\t${token.attr[PosTag].categoryValue}")}
  }
}
