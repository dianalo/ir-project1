package ch.ethz.ir.project1

import ch.ethz.dal.tinyir.io._

import breeze.linalg._
import scala.math._

class NaiveBayesLec (val config: Config, 
                      val threshold:Double, var Pcat: Array[Double], var logPwc: Array[Map[String, Double]]){
  
  //encoding of codes in vector: region, topic, industry
  
  def computeProbabilities(trainingDataFolder: String) = {
      val reuters = new ReutersRCVStream(trainingDataFolder)
      val stream = reuters.stream
      val vocabSize = stream.flatMap(_.tokens).distinct.length
      val codes = config.codes
      for(i <- 0 to codes.size-1){
        val cat = codes(i)
        val v = log(stream.filter(_.codes(cat)).length /stream.length.toDouble)
        Pcat(i) = v
        val tks = stream.filter(_.codes(cat)).flatMap(_.tokens)
        val denominator = tks.length.toDouble + vocabSize
        val PwcSparseNumerator = tks.groupBy(identity).mapValues(l=>l.length+1)
        logPwc(i) = PwcSparseNumerator.mapValues { v => log(v/denominator) }+("_df" -> log(1.0/denominator)) //default value
        println((i.toDouble/codes.size.toDouble)*100 + "% computed")
      }
  }
  
  def classify(dataFolder: String) : List[List[String]] = {
    var ls: List[List[String]] = List()
    val stream = new ReutersRCVStream(dataFolder).stream
    val codes = config.codes
    var topicScores: Array[Double] = new Array[Double](codes.size)
    var j=0
    for (doc <- stream){
      val tks = doc.tokens
      val tfs = tks.groupBy(identity).mapValues(l=>l.length)
      for(i <- 0 to codes.size-1){
        var sum = 0.0
        for(word <- tfs.keys){
          sum += tfs(word)*logPwc(i).getOrElse(word, logPwc(i)("_df"))
        }
        sum += Pcat(i)
        topicScores(i) = sum
      }
      
      //get single best labels... maybe multiple labels via thresholding
      //filter region
      val region = config.invCodeDictionnary(topicScores.slice(0, config.nRegionCodes-1).zipWithIndex.maxBy(_._1)._2)
      //filter topic
      val topic = config.invCodeDictionnary(topicScores.slice(config.nRegionCodes, config.nRegionCodes+config.nTopicCodes-1).zipWithIndex.maxBy(_._1)._2)
      //filter industry
      val industry = config.invCodeDictionnary(topicScores.slice(config.nRegionCodes+config.nTopicCodes, codes.size-1).zipWithIndex.maxBy(_._1)._2)
      
      val l = List(region, topic, industry)
      
      ls = l::ls
      
      j+=1
      println((j.toDouble/stream.length.toDouble)*100 + "% classified")
    }
    return ls
  }
}