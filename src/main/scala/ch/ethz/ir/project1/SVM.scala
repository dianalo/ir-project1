package ch.ethz.ir.project1

import ch.ethz.dal.tinyir.processing.ReutersRCVParse
import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.lectures.SMap
import scala.util.Random
import scala.collection.mutable.HashMap
import ch.ethz.dal.tinyir.processing.StopWords
import com.github.aztek.porterstemmer.PorterStemmer

class SVM(config: Config, var theta: Array[SMap], lambda: Double, NumberOfTrainingFiles: Int) {
  
  //extracts the feature vector out of a XML document
  /*currently the feature vector only contains the frequencies
   * of terms present in the document
   */
  def extractFeatureVector(d: XMLDocument) : Map[String, Double] = {
    val tks = StopWords.filterOutSW(d.tokens).map(PorterStemmer.stem(_))
    val tfs = tks.groupBy(identity).mapValues { l => l.length.toDouble }
    
    return tfs
  }
  
  //addition of two SMaps
  def plus(a: SMap, b: SMap) : SMap = {
    var res = Map[String, Double]()
    for(key <- a.m.keySet.union(b.m.keySet)){
      res += (key -> (a.m.getOrElse(key, 0.0) + b.m.getOrElse(key, 0.0)))
    }
    return new SMap(res)
  }
  
  //update function for our model vectors, from PEGASOS algorithm
  //see slide Text Categorization, Slide No. 37
  //params: 
  //th: SMap with model vectors for label l
  //x: SMap with training data
  //step: iteration step
  //c: boolean indicating whether current training document has label l
  def update(th: SMap, x: SMap, step: Int, c: Boolean) = {
    val y = if (c) 1 else -1
    if(y*(th*x) >= 1){
      th*(1.0 - 1.0/(lambda*step)*lambda)
    }
    else{
      plus(th*(1.0 - 1.0/(lambda*step)*lambda),(x*y)*(1.0 - 1.0/(lambda*step)*lambda))
    }
  }
  
  /*	computes the model vectors for the different labels 
   * 	by looking at training data according to PEGASOS algorithm
   * 	return and stores the computed model vectors in theta array of SMaps
   */
  //params:
  //trainingDataFolder: the path of the folder containing the training data
  //iterations: number of iterations to be performed
  def computeParameters(trainingDataFolder: String, iterations: Int) : Array[SMap] = {
    var str = new ReutersRCVStream(trainingDataFolder).stream
    val features = str.map {d => (extractFeatureVector(d), d.codes)}
    val nCodes = config.codes.length
    
    val docList = str.toList;
    
    //iterate over it randomly
    for(j <- 0 to iterations){
      for(i <- 0 to config.codes.size-1){
        var d = docList(Random.nextInt(NumberOfTrainingFiles-1))
        var x = new SMap(extractFeatureVector(d))
        val c = d.codes.contains(config.invCodeDictionnary(i))
        //perform update step
        theta(i) = update(theta(i), x, j+1, c)
      }
      println("Computing parameters: " + (j.toDouble/iterations.toDouble)*100.0 + "% complete.")
    }
    
    return theta
  }
  
  //classifies documents in folder
  //returns:
  //list of tuples containing the Document ID and a List of all identifies labels
  //the returned list has the same order as the read in documents
  def classify(validationDataFolder: String) : List[(Int, List[String])] = {
    val stream = new ReutersRCVStream(validationDataFolder).stream
    return stream.map(classifyDoc(_)).foldLeft(List[(Int, List[String])]())((a,b)=>b::a).reverse
  }
  
  //helper function that classifies one single document
  private def classifyDoc(d: XMLDocument) :  (Int, List[String]) = {
    val feature = new SMap(extractFeatureVector(d))
    val res = theta.map(_*feature).zipWithIndex.filter(_._1 > 0.0)
    
    return (d.ID, res.toList.map{ t => config.invCodeDictionnary(t._2)})
  }
  
}