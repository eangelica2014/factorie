package cc.factorie.app.nlp.languageIdentify

//import cc.factorie.app.classify.backend.TestNaiveBayes.GenderDomain._

import java.io.{BufferedOutputStream, File}
import java.nio.charset.CodingErrorAction

import cc.factorie.app.nlp.pos.ForwardPosOptions
import cc.factorie.util.{JavaHashMap, CubbieConversions, BinarySerializer}
import cc.factorie.variable.{FeatureVectorVariable, CategoricalVectorDomain, CategoricalDomain}
import cc.factorie.app.nlp.{SharedNLPCmdOptions, load}
import com.sun.xml.internal.fastinfoset.util.CharArray


import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.io.{Source, Codec}


/**
 * Created by Esther on 8/10/15.
 */

object langId {
  val NODE = 3
  val EDGE = 6

  val node_training_docs=ListBuffer[(mutable.Map[String,Double], String)]()
  val edge_training_docs=ListBuffer[(mutable.Map[String,Double], String)]()

  def grab_data(file_path:String): mutable.Map[String,String] ={
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

    return documents
  }
  def create_counts(n:Int, data:String, lang:String):(mutable.Map[String, Double], String) ={
    val d = data
    val a=d.length

    //figure out how to SCALA-fy this...
    var i = 0
    var map = mutable.Map[String, Double]()
    while(i<a-(n-1)){
      if(map.contains(d.substring(i,i+n))){
        map(d.substring(i,i+n))+= 1
      }
      else{
        map+=(d.substring(i,i+n) -> 1)
      }
      i+=1
    }

    return (map, lang)
  }

  def create_counts(n:Int,data:mutable.Map[String,String], lang:String): (mutable.Map[String, Double], String) ={
    val d = data.get(lang).mkString
    val a=d.length


    //println("Size of string:" + a)

    //figure out how to SCALA-fy this...
    var i = 0
    var map = mutable.Map[String, Double]()
    while(i<(a-(n-1))){
      if(map.contains(d.substring(i,i+n))){
        map(d.substring(i,i+n))+= 1
      }
      else{
        map+=(d.substring(i,i+n) -> 1)
      }
      i+=1
    }

    //println("Size of map:" + map.keySet.size)

    return (map, lang)
  }

  def score(node_counts:ListBuffer[(mutable.Map[String, Double], String)], edge_counts:ListBuffer[(mutable.Map[String, Double], String)], test_node:(mutable.Map[String, Double], String), test_edge:(mutable.Map[String, Double], String)): ListBuffer[(Double, String)]={
    val scores = ListBuffer[(Double, String)]()
    val node_scores = ListBuffer[(Double, String)]()
    val edge_scores = ListBuffer[(Double, String)]()

    //println("Node counts size: " + node_counts.size)

    for(i <- node_counts){
      //total - the sum of all n-grams in node_counts (training data)
      //println("Inside loop map size:" + i._1.keySet.size)
      //val total = i._1.foldLeft(0.0)(_+_._2)
      //for(j <- i._1.keySet){
        /*
        if(i._1(j) < 1){
          println(j)
          println("What happened!")
        }*/
        //change the values in the hashmap to the percentage of all n-grams
        //i._1.update(j, i._1(j)/ total)
      //}

      for(k <- test_node._1.keySet){
        test_node._1.update(k,i._1.getOrElse(k,0))
      }

      //scores.+(test.foldLeft(0.0)(_+_._2))
      val tuple = (test_node._1.foldLeft(0.0)(_+_._2), i._2)
      node_scores += tuple
      //println(test._1.foldLeft(0.0)(_+_._2))

    }

    for(i <- edge_counts){
      //val total = i._1.foldLeft(0.0)(_+_._2)
      /*for(j <- i._1.keySet){
        i._1.update(j, scala.math.log(i._1(j))/scala.math.log(total))
      }*/
      for(k <- test_edge._1.keySet){
        test_edge._1.update(k,i._1.getOrElse(k,0))
      }

      //scores.+(test.foldLeft(0.0)(_+_._2))
      val tuple = (test_edge._1.foldLeft(0.0)(_+_._2), i._2)
      edge_scores += tuple
      //println(test._1.foldLeft(0.0)(_+_._2))

    }
    for(i <- 0 to node_scores.length-1) {
      val tuple = (edge_scores(i)._1 + node_scores(i)._1, node_scores(i)._2)
      scores += tuple
    }

    return scores
  }


  def create_model(docs: mutable.Map[String,String]): Unit ={
    for(d <- docs.keySet){
      node_training_docs += create_counts(NODE, docs, d)
      edge_training_docs += create_counts(EDGE, docs, d)
    }
    for(i <- node_training_docs) {
      //total - the sum of all n-grams in node_counts (training data)
      //println("Inside loop map size:" + i._1.keySet.size)
      val total = scala.math.log(i._1.foldLeft(0.0)(_ + _._2))
      for (j <- i._1.keySet) {

        if(i._1(j) < 1){
          //println(j)
          //println("What happened!")
        }
        //change the values in the hashmap to the percentage of all n-grams
        i._1.update(j, scala.math.log(i._1(j)) / total)
      }
    }
    for(i <- edge_training_docs) {
      val total = scala.math.log(i._1.foldLeft(0.0)(_ + _._2))
      for (j <- i._1.keySet) {
        i._1.update(j, scala.math.log(i._1(j)) / total)
      }
  }
}

def test_probabilities(test: mutable.Map[String, String], t:String): (Double,String) ={
var max = (0.0, "unknown")
for(p <- score(node_training_docs,edge_training_docs,create_counts(NODE, test, t), create_counts(EDGE, test, t))){
//println("Hello World")
if(p._1 >= max._1) {
  max = p
}
//println(p)
}
return max
}
def test_probabilities(test: String, t:String): (Double,String) ={
var max = (0.0, "unknown")
for(p <- score(node_training_docs,edge_training_docs,create_counts(NODE, test, t), create_counts(EDGE, test, t))){
//println("Hello World")
if(p._1 >= max._1) {
  max = p
}
//println(p)
}
//println("Max: " + max)
return max
}

def test_accuracies(test: mutable.Map[String,String]): ListBuffer[Double] ={
var accuracies = new ListBuffer[Double]
for(t <- test.keySet) {
println("Processing Label: " + t + "...")
val test_array = test.get(t).mkString.split("\\r?\\n")
var results = new Array[(Double, String)](test_array.length)
var i = 0
for (word <- test_array) {
  /*if(i%100 == 0) {
    println(i)
  }*/
  //println(word.toUpperCase)
  results{i} = test_probabilities(word, t)
  //println(results{i})
  i += 1
}
var j = 0;
var total = 0
for(max <- results) {
  if (max._2 == t) {
    total += 1
  } else{
    println("Word: " + test_array{j}.toUpperCase)
    println("True Label: " + t + " " + "Predicted Label: " + max._2)
  }
  j+=1
}
accuracies.append(total.toDouble/test_array.length.toDouble)
}

return accuracies
}

def print_accuracies(test: mutable.Map[String,String]): Unit ={
var k=0
val accuracies = test_accuracies(test)
for(t<-test.keySet){
println(t.toUpperCase + ": " + accuracies(k))
k+=1
}
}

def main(args: Array[String]): Unit = {

val opts = new LangIdOptions
opts.parse(args)

assert(opts.trainDir.wasInvoked && opts.testDir.wasInvoked)
val training_dir = opts.trainDir.value
val testing_dir = opts.testDir.value

val docs = this.grab_data(training_dir)
val test = this.grab_data(testing_dir)

println("Creating model...")
create_model(docs)
println("Done creating model.")
println("Getting accuracy...")
print_accuracies(test)

/*var totals = new ListBuffer[Int]
for(t <- test.keySet) {
val test_array = test.get(t).mkString.split("\\r?\\n")
var results = new Array[(Double, String)](test_array.length)
var i = 0
for (word <- test_array) {
  //println(word.toUpperCase)
  results{i} = test_probabilities(word, t)
  i += 1
}

var total = 0
for(max <- results) {
  if (max._2 == t) {
    total += 1
  }
}
totals.append(total)
}

totals.foreach(println)*/


//val max_array = test_array.foreach(test_probabilities)


// val node_training_docs=ListBuffer[(mutable.Map[String,Double], String)]()
//val edge_training_docs=ListBuffer[(mutable.Map[String,Double], String)]()

/*for(d <- docs.keySet){
println(d)
//println(create_counts(NODE, docs, d))
node_training_docs += create_counts(NODE, docs, d)
edge_training_docs += create_counts(EDGE, docs, d)
}*/
//println(training_docs)
//println("_____________")
//println(score(training_docs,create_counts(NODE, test, "test")))
/*var max = (0.0, "unknown")
for(p <- score(node_training_docs,edge_training_docs,create_counts(NODE, test, "test"), create_counts(EDGE, test, "test"))){
//println("Hello World")
if(p._1 >= max._1) {
  max = p
}
println(p)
}*/

//println("max: " + max._2)


/*
val m = create_counts(NODE,docs,"jav")
val h = create_counts(NODE,docs,"eng")
val v = m.foldLeft(0.0)(_+_._2)
val x = h.foldLeft(0.0)(_+_._2)

for(i <- m.keySet) {
m.update(i, math.log(m(i)) / v)
}

for(i <- h.keySet) {
h.update(i, math.log(h(i)) / v)
}


/*CLASSIFY*/

val jav_test_counts = create_counts(NODE, test, "test")
for(j <- jav_test_counts.keySet){
jav_test_counts.update(j,m(j)*jav_test_counts(j))
}

val jav_score = jav_test_counts.foldLeft(0.0)(_+_._2)

val eng_test_counts = create_counts(NODE, test, "test")
for(j <- eng_test_counts.keySet){
eng_test_counts.update(j,h(j)*eng_test_counts(j))
}

val eng_score = eng_test_counts.foldLeft(0.0)(_+_._2)

println("jav score: " + jav_score)
println("eng score:" + eng_score)
*/

/*
for(k <- docs.keySet) {
println(k)
println("NODES")
create_counts(NODE, docs, k)
println("EDGES")
create_counts(EDGE, docs,k)
println("")
}

println(test)
println("NODES")
create_counts(NODE,test,"test")
println("EDGES")
create_counts(EDGE,test,"test")
println("")
*/
println("DONE")

}

}

class LangIdOptions extends cc.factorie.util.DefaultCmdOptions with SharedNLPCmdOptions {
val modelFile = new CmdOption("model", "", "FILENAME", "Filename for the model (saving a trained model or reading a running model.")
val testDir = new CmdOption("test-dir", "", "FILENAME", "OWPL test file.")
val trainDir = new CmdOption("train-dir", "", "DIRNAME", "Get directory of training files")
val saveModel = new CmdOption("save-model", "LangId.factorie", "FILENAME", "Filename for the model (saving a trained model or reading a running model.")
}




