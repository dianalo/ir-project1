package ch.ethz.ir.project1

import ch.ethz.dal.tinyir.lectures._

import java.io.File
import java.io.PrintWriter

//app to compute the Logistic Regression model parameters for a set of documents
//and then classify another set of documents
//parameters needed to run are annotated in the source code below

object LogisticRegressionScript extends App {
  override def main(args: Array[String]){
    val config = new Config
    
     //initialize all theta maps, i.e. array with sparse map representing model vector for each label
    //needed for our one-vs-all classification approach
    var theta: Array[SMap] = new Array[SMap](config.codes.length)
    for(i <- 0 to config.codes.length-1){
      theta(i) = new SMap(Map[String, Double]())
    }
    
    //PARAM 3:
    //GLOBAL THRESHOLD USED BY LOGISTIC REGRESSION CLASSIFICATION
    var lR = new LogisticRegression(config, theta, 0.3)
    
    println("computing parameters...")
    ///PARAMS:
    //PATH TO TRAINING FOLDER
    //NUMBER OF ITERATIONS
    //LEARNING RATE PARAMETER (FROM LR-ALGORITHM)
    theta = lR.computeParameters("resources/trainSmall/zips", 100, 1/config.nDocs)
    println("classifying...")
    //PARAM:
    //PATH TO VALIDATION/TEST FOLDER
    val res = lR.classify("resources/validationSmall/zips")
    
    println("ID\tCODES")
     var t = System.currentTimeMillis()
     //PARAM:
     //PATH TO FILE WHERE RESULTS ARE STORED
     var f = new File("resources/FoundTopicLabels/resultLR"+t)
     f.createNewFile()
     val pw = new PrintWriter("resources/FoundTopicLabels/resultLR"+t)
     for(t <- res){
       //t: (ID, List of Codes) 
       println(t._1 + " " + t._2.mkString("\t"))
       pw.println(t._1 + " " + t._2.mkString("\t"))
     }

     pw.flush()
     pw.close()
  }
}