package ch.ethz.ir.project1

import ch.ethz.dal.tinyir.processing.ReutersRCVParse
import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.lectures.SMap
import scala.util.Random
import scala.collection.mutable.HashMap

import breeze.linalg.DenseVector

class SVM(config: Config, var theta: Array[SMap], lambda: Double) {
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
  
  def update(th: SMap, x: SMap, step: Int, c: Boolean) = {
    val y = if (c) 1 else -1
    if(y*(th*x) >= 1){
      th*(1.0 - 1.0/(lambda*step)*lambda)
    }
    else{
      plus(th*(1.0 - 1.0/(lambda*step)*lambda),(x*y)*(1.0 - 1.0/(lambda*step)*lambda))
    }
  }
  
  def computeParameters(trainingDataFolder: String, iterations: Int) : Array[SMap] = {
    var str = new ReutersRCVStream(trainingDataFolder).stream
    val features = str.map {d => (extractFeatureVector(d), d.codes)}
    val nCodes = config.codes.length
    
    //-> stochastic optimization, walk through data in random order
    //val randStr = Stream.fill(iterations)(Random.nextInt(config.nDocs-1))
    val docList = str.toList;
    
    //iterate over it randomly
    for(j <- 0 to iterations){
      for(i <- 0 to config.codes.size-1){
        //println(docList.length)
        var d = docList(Random.nextInt(config.nDocsSmall))
        val f = extractFeatureVector(d)
        var x = new SMap(f)
        val c = d.codes.contains(config.invCodeDictionnary(i))
        theta(i) = update(theta(i), x, j+1, c)
      }
      println("Computing parameters: " + (j.toDouble/iterations.toDouble)*100.0 + "% complete.")
    }
    
    return theta
  }
    
  def classify(validationDataFolder: String) : List[(Int, List[String])] = {
    val stream = new ReutersRCVStream(validationDataFolder).stream
    return stream.map(classifyDoc(_)).foldLeft(List[(Int, List[String])]())((a,b)=>b::a).reverse
  }
  
  private def classifyDoc(d: XMLDocument) :  (Int, List[String]) = {
    val feature = new SMap(extractFeatureVector(d))
    println(theta.map(_*feature).apply(0))
    val res = theta.map(_*feature).zipWithIndex.filter(_._1 > 0.0)
    
    return (d.ID, res.toList.map{ t => config.invCodeDictionnary(t._2)})
  }
  
}