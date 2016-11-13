package ch.ethz.ir.project1

import ch.ethz.dal.tinyir.io._

import breeze.linalg._
import scala.math._

/*
 * NaiveBayes Class
 * provides all the functions needed to compute parameters and classify
 * according to NaiveBayes classifier, pretty much the same as on the slides
 * params:
 * config: configuration object
 * threshold: global threshold to classify document
 * Pcat: array where probabilities P(c) are stored
 * Pwc: matrix where probabilites P(w|c) are stored for each class/label
 * */
class NaiveBayesLec (val config: Config, 
                      val threshold:Double, var Pcat: Array[Double], var Pwc: Array[Map[String, Double]]){
  
  
  /*
   * computes probabilites P(c) and P(w|c) for given training data
   * 
   * */ 
  def computeProbabilities(trainingDataFolder: String) = {
      val reuters = new ReutersRCVStream(trainingDataFolder)
      val stream = reuters.stream
      //vocabsize could be hardcoded for full training set
      val vocabSize = stream.flatMap(_.tokens).distinct.length
      val codes = config.codes
      for(i <- 0 to codes.size-1){
        val cat = codes(i)
        val docsInCat = stream.filter(_.codes(cat))
        //log not really needed
        val v:Double = docsInCat.length.toDouble / stream.length.toDouble
        Pcat(i) = v
        val tks = docsInCat.flatMap(_.tokens)
        val denominator: Double = tks.length.toDouble + vocabSize.toDouble
        val PwcSparseNumerator = tks.groupBy(identity).mapValues(l=>l.length+1)
        //log not really needed
        Pwc(i) = PwcSparseNumerator.mapValues { v => v.toDouble/denominator}+("_df" -> 1.0/denominator) //default value
        println((i.toDouble/codes.size.toDouble)*100 + "% computed")
      }
  }
  
  /*
   * classifies documents in given folder
   * 
   * returns:
   * list of tuples containing the Document ID and a List of all identified labels
   * the returned list has the same order as the read in documents
   * */
  def classify(dataFolder: String) : List[(Int,List[String])] = {
    var ls: List[(Int,List[String])] = List()
    val stream = new ReutersRCVStream(dataFolder).stream
    val codes = config.codes
    var topicScores: Array[Double] = new Array[Double](codes.size)
    var j=0
    for (doc <- stream){
      val tfs = doc.tokens.groupBy(identity).mapValues(l=>l.length)
      for(i <- 0 to codes.size-1){
        var sum = 0.0
        for(word <- tfs.keys){
          sum += tfs(word)*Pwc(i).getOrElse(word, Pwc(i)("_df"))
        }
        sum += Pcat(i)
        topicScores(i) = sum
      }
           
      val assignedTopics = topicScores.zipWithIndex.filter(_._1 > threshold).map(s => config.invCodeDictionnary(s._2))
          
      //val l = List(region, topic, industry)
      val l = (doc.ID, assignedTopics.toList)
      
      ls = l::ls
      
      j+=1
      println((j.toDouble/stream.length.toDouble)*100 + "% classified")
    }
    return ls.reverse
  }
}