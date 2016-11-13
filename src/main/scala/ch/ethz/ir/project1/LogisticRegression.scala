package ch.ethz.ir.project1

import ch.ethz.dal.tinyir.processing.ReutersRCVParse
import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.lectures._
import scala.util.Random

/*
 * LogisticRegression class
 * 
 * params:
 * config: configuration object containing configuration data shared between different classes
 * theta: array containing the model vectors of all class labels
 * threshold: global threshold used to classify documents
 * */
class LogisticRegression(config: Config, var theta: Array[SMap], threshold: Double) {
  
  //extracts the feature vector out of a XML document
  /*currently the feature vector only contains the frequencies
   * of terms present in the document
   */  
  def extractFeatureVector(d: XMLDocument) : Map[String, Double] = {
    val tks = d.tokens
    val tfs = tks.groupBy(identity).mapValues { l => l.length.toDouble }
    
    return tfs
  }
  
  //returns result of subtraction a-b of two SMaps
  def minus(a: SMap, b: SMap) : SMap = {
    var res = Map[String, Double]()
    for(key <- a.m.keySet.union(b.m.keySet)){
      res += (key -> (a.m.getOrElse(key, 0.0) - b.m.getOrElse(key, 0.0)))
    }
    return new SMap(res)
  }
  
  
  //computes model vectors for each class label by looking at training data
  /*
   * trainingDataFolder: path to folder containing training data
   * iterations: number of iterations to be performed
   * learningRate: parameter that regularizes conversion speed of algorithm
   * */
  def computeParameters(trainingDataFolder: String, iterations: Int, learningRate: Double) : Array[SMap] = {
    var str = new ReutersRCVStream(trainingDataFolder).stream
    val features = str.map {d => (extractFeatureVector(d), d.codes)}
    val nCodes = config.codes.length
    //reusing functionality in tinyir library
    val lr = new ch.ethz.dal.tinyir.lectures.LogisticRegression()
    //need to transform to list to be able to walk over data randomly
    val docList = str.toList
    var j=0
    for(j <- 0 to iterations){
      //select next element randomly
      var d = docList(Random.nextInt(config.nDocs))
        val fV = new SMap(extractFeatureVector(d))
        for(i <- 0 to config.codes.size-1){
          //perform update step
          theta(i) = minus(theta(i), lr.update(theta(i), fV, d.codes.contains(config.codes(i)))*learningRate)
        }
        println("computing parameters: " + (j.toDouble/str.size.toDouble)*100 + "% done")
    }
    
    return theta
  }
  
  
  //classifies data in a folder
  //returns:
  //list of tuples containing the Document ID and a List of all identified labels
  //the returned list has the same order as the read in documents
  def classify(validationDataFolder: String) : List[(Int, List[String])] = {
    val stream = new ReutersRCVStream(validationDataFolder).stream
    return stream.map(classifyDoc(_)).foldLeft(List[(Int, List[String])]())((a,b)=>b::a).reverse
  }
  
  //helper function that classifies one single document
  private def classifyDoc(d: XMLDocument) :  (Int, List[String]) = {
    val feature = new SMap(extractFeatureVector(d))
    val lr = new ch.ethz.dal.tinyir.lectures.LogisticRegression()
    val res = theta.map(lr.logistic(feature, _)).zipWithIndex.filter(_._1 > threshold)
    
    return (d.ID, res.toList.map{ t => config.invCodeDictionnary(t._2)})
  }
}