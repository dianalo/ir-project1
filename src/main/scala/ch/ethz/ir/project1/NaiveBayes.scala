package ch.ethz.ir.project1

import ch.ethz.dal.tinyir.processing.ReutersRCVParse
import ch.ethz.dal.tinyir.io.ReutersRCVStream

import ch.ethz.dal.tinyir.io.DocStream

import breeze.linalg._
import breeze.numerics._

import scala.collection.mutable._

class NaiveBayes(fileList: List[String], topicMap: collection.mutable.Map[String, List[Any]], 
    totalDocs: Int, alpha: Double, threshold: Double, var logPwcNum: Map[String, Map[String, Option[Double]]],
    var logPwcDenom: Map[String, Double], var initialized: Boolean) {
    
  //encoding of codes in vector: region, topic, industry
  
  def initialize = {
    logPwcNum = PwcNumeratorWithSmoothing
    logPwcDenom = PwcDenominatorWithSmoothing
    initialized = true
  }
  
  def classify(d : ReutersRCVParse) : List[String] = {
    
    if(!initialized){
      initialize
    }
    
    var wMap = new HashMap[String, Int]()
    
    for(t <- d.tokens){
      wMap(t) += 1
    }
    
    var res = new HashMap[String, Double]()
    
    for(c <- topicMap.keys){
      for(w <- wMap.keys){
        res(c) = wMap(w).toDouble * logPwc(w,c)
      }
    }
    
    return res.filter(_._2 > threshold).keys.toList
  }
  
  //computes log(P(c)) vector
  def logPc : DenseVector[Double] = {
    var docCounts = topicMap.clone().mapValues { _.size}.asInstanceOf[collection.mutable.Map[String, Double]]
    for(fName <- fileList){
      val parse = new ReutersRCVParse(DocStream.getStream(fName))
      val codes = parse.codes
      for(c <- codes){
        docCounts(c) += 1.0
      }      
    }
        
    var docCountVec = DenseVector.create(docCounts.values.toArray, 0, 0, docCounts.values.size) / DenseVector.fill(docCounts.values.size){totalDocs.toDouble}
    var logP = log(docCountVec)
    
    return logP
  }
  
  def logPwc(w:String, c:String) : Double = {
    val num = logPwcNum(c)(w).getOrElse(1.0)
    val denom = logPwcDenom(c)
    
    return scala.math.log(num/denom)
  }
  
  //computes the numerator of log(P(w|c)) and stores it for every class in a word Map
  def PwcNumeratorWithSmoothing : Map[String, Map[String, Option[Double]]] = {
    var cMap = topicMap.clone().asInstanceOf[LinkedHashMap[String, Map[String, Double]]]
    var wMap = new HashMap[String, Int]()
    for(fName <- fileList){
      val parse = new ReutersRCVParse(DocStream.getStream(fName))
      //compute term frequencies for given document
      for(token <- parse.tokens){
        wMap(token) += 1 
      }
      //add those term frequencies to all topics this document is annotated
      for(code <- parse.codes){
        for((key,value) <- wMap){
          cMap(code)(key) += value.toDouble 
        }
      }
    }
    //add alpha aswell
    return cMap.mapValues{_.mapValues{v => Some(v+alpha)}}.asInstanceOf[scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, Option[Double]]]]
  }
  
  def PwcDenominatorWithSmoothing : Map[String, Double] = {
    //map to store denominator value per class
    var cMap = topicMap.clone().mapValues{_.size.toDouble}.asInstanceOf[LinkedHashMap[String, Double]]
    //set to count differing words
    var wSet = new HashSet[String]()
    for(fName <- fileList){
      val parse = new ReutersRCVParse(DocStream.getStream(fName))
      for(t <- parse.tokens){
        wSet.add(t)
      }
      for(c <- parse.codes){
        cMap(c) += parse.tokens.size.toDouble
      }
    }
    
    var totDiffW = wSet.size
    
    for(key <- cMap.keys){
      cMap(key) += alpha * totDiffW
    }
    
    return cMap
  }
  
    
}
