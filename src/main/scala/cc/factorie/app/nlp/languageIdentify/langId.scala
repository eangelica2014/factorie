package cc.factorie.app.nlp.languageIdentify

//import cc.factorie.app.classify.backend.TestNaiveBayes.GenderDomain._

import java.io.File
import java.nio.charset.CodingErrorAction

import cc.factorie.app.nlp.pos.ForwardPosOptions
import cc.factorie.variable.{FeatureVectorVariable, CategoricalVectorDomain, CategoricalDomain}
import cc.factorie.app.nlp.load


import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.{Source, Codec}


/**
 * Created by Esther on 8/10/15.
 */
object langId {
  def grab_data(file_path:String): Map[String,String] ={
    val decoder = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.IGNORE)
    val dir = new File(file_path)
    var documents = mutable.Map[String, String]()
    for(file <- dir.listFiles()) {
      //filter out pesky mac file .DS_Store
      if (!file.getName().contains("DS_Store")) {
        //map the filename to the contents of the file
        documents += (file.getName() -> Source.fromFile(file)(Codec.decoder2codec(decoder)).mkString)
      }
    }

    return documents.toMap
  }


  def main(args: Array[String]): Unit = {

    val docs = this.grab_data("/Users/Esther/Documents/Summer2015/Language_Id/Data")
    println(docs("jav"))

  }


}
