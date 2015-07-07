package cc.factorie.app.nlp.load

import cc.factorie.app.nlp.pos.{PosTag, LabeledGermanPosTag}
import cc.factorie.app.nlp.{Sentence, UnknownDocumentAnnotator, Token, Document}
import cc.factorie.app.nlp.pos
/**
 * Created by Esther on 6/18/15.
 */
object LoadTigerConll09 extends Load {

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
    var document = newDocument("TigerConll09" + documents.length)
    documents += document
    var sentence = new Sentence(document)
    for (line <- source.getLines()) {
      if (line.length < 2) {
        document.appendString("\n")
        sentence = new Sentence(document)
      }
      else {
        val fields = line.split('\t')
        assert(fields.length == 15)
        val word = fields(1)
        val partOfSpeech = fields(4)
        if (sentence.length > 0) document.appendString(" ")
        val token = new Token(sentence, word)
        token.attr += new LabeledGermanPosTag(token, partOfSpeech)
      }
    }
    documents
  }

  def main(args: Array[String]) = {
    // here we assume first arg is just the data filename
    val fname = args(0)
    val docs = fromFilename(fname)

    // just print out the first document loaded, one word per line
    docs.head.tokens.foreach{token => println(s"${token.string}\t${token.attr[PosTag].categoryValue}")}
  }
}
