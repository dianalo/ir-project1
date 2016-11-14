package ch.ethz.ir.project1

import ch.ethz.dal.tinyir.lectures.SMap
import java.io.File
import java.io.PrintWriter

object VaryingClassificationParameters extends App {
  override def main(args: Array[String]){
    val config = new Config
    
    var trainingFiles = "resources/trainVerySmall/zips"
    var validationFiles = "resources/validationSmall/zips"
    
    //compute parameters
     
     //initialize naive bayes
     var Pcat: Array[Double] = new Array[Double](config.codes.size)
     var logPwc: Array[Map[String, Double]] = new Array[Map[String, Double]](config.codes.size)
     var nB = new NaiveBayesLec(config, Pcat, logPwc)
     nB.computeProbabilities(trainingFiles)
     
    //initialize LR
    var theta: Array[SMap] = new Array[SMap](config.codes.length)
    for(i <- 0 to config.codes.length-1){
      theta(i) = new SMap(Map[String, Double]())
    }
    
    var lR = new LogisticRegression(config, theta, 10)
    lR.computeParameters(trainingFiles, 25)
    
    
    var threshold = 0.2
    while(threshold < 0.8){
      val resNB = nB.classify(validationFiles, threshold)
      val resLR = lR.classify(validationFiles, threshold)
      
      //write data of NB to a file
     var fNB = new File("resources/FoundTopicLabels/resultNB_THRESHOLD_" + threshold)
     fNB.createNewFile()
     val pwNB = new PrintWriter("resources/FoundTopicLabels/resultNB_THRESHOLD_" + threshold)
     for(t <- resNB){
       pwNB.println(t._1 + " " + t._2.mkString("\t"))
     }
     pwNB.flush()
     pwNB.close()
     
      //write data of LR to a file
     var fLR = new File("resources/FoundTopicLabels/resultLR_THRESHOLD_" + threshold)
     fLR.createNewFile()
     val pwLR = new PrintWriter("resources/FoundTopicLabels/resultLR_THRESHOLD_" + threshold)
     for(t <- resLR){
       pwLR.println(t._1 + " " + t._2.mkString("\t"))
     }
     pwLR.flush()
     pwLR.close()
     
     threshold += 0.05
    }
  }
}