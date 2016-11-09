package ch.ethz.ir.project1

import ch.ethz.dal.tinyir.processing.ReutersRCVParse
import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.lectures._
import scala.util.Random


class LogisticRegression(config: Config, var theta: Array[SMap], threshold: Double) {
  
  //maybe consider more features than term frequencies...
  def extractFeatureVector(d: XMLDocument) : Map[String, Double] = {
    val tks = d.tokens
    val tfs = tks.groupBy(identity).mapValues { l => l.length.toDouble }
    
    return tfs
  }
  
  def plus(a: SMap, b: SMap) : SMap = {
    var res = Map[String, Double]()
    for(key <- a.m.keySet.union(b.m.keySet)){
      res += (key -> (a.m.getOrElse(key, 0.0) + b.m.getOrElse(key, 0.0)))
    }
    return new SMap(res)
  }
  
  def minus(a: SMap, b: SMap) : SMap = {
    var res = Map[String, Double]()
    for(key <- a.m.keySet.union(b.m.keySet)){
      res += (key -> (a.m.getOrElse(key, 0.0) - b.m.getOrElse(key, 0.0)))
    }
    return new SMap(res)
  }
  
  def computeParameters(trainingDataFolder: String, iterations: Int, learningRate: Double) : Array[SMap] = {
    var str = new ReutersRCVStream(trainingDataFolder).stream
    val features = str.map {d => (extractFeatureVector(d), d.codes)}
    val nCodes = config.codes.length
    val lr = new ch.ethz.dal.tinyir.lectures.LogisticRegression()
    //-> stochastic optimization, walk through data in random order
    //val randStr = Stream.fill(iterations)(Random.nextInt(nCodes-1))
    //for(ri <- randStr){
    var j=0
    for(d <- str){
      //val d = str.take(ri).last 
      val fV = new SMap(extractFeatureVector(d))
      for(i <- 0 to config.codes.size-1){
        theta(i) = minus(theta(i), lr.update(theta(i), fV, d.codes.contains(config.codes(i)))*learningRate)
      }
      j = j+1
      println("computing parameters: " + (j.toDouble/str.size.toDouble)*100 + "% done")
    }
    
    return theta
  }
  
  def classify(validationDataFolder: String) : List[(Int, List[String])] = {
    val stream = new ReutersRCVStream(validationDataFolder).stream
    return stream.map(classifyDoc(_)).foldLeft(List[(Int, List[String])]())((a,b)=>b::a).reverse
  }
  
  //preserve ID as needed in result file 
  private def classifyDoc(d: XMLDocument) :  (Int, List[String]) = {
    val feature = new SMap(extractFeatureVector(d))
    val lr = new ch.ethz.dal.tinyir.lectures.LogisticRegression()
    val res = theta.map(lr.logistic(feature, _)).zipWithIndex.filter(_._1 > threshold)
    
    return (d.ID, res.toList.map{ t => config.invCodeDictionnary(t._2)})
  }
}