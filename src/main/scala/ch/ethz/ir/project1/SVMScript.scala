package ch.ethz.ir.project1

import ch.ethz.dal.tinyir.lectures._
import java.io._

//app to compute the SVM model parameters for a set of documents
//and then classify another set of documents
//parameters needed to run are annotated in the source code below

object SVMScript extends App {
  override def main(args: Array[String]) = {
    //import the configuration data
    val config = new Config
    
    //initialize all theta maps, i.e. array with sparse map representing model vector for each label
    //needed for our one-vs-all classification approach
    var theta: Array[SMap] = new Array[SMap](config.codes.length)
    for(i <- 0 to config.codes.length-1){
      theta(i) = new SMap(Map[String, Double]())
    }
    
    
    //PARAM 3:
    //LAMBDA PARAMETER FROM PEGASOS ALGORITHM
    var svm = new SVM(config, theta, 1.0)
    
    println("computing parameters...")
    //PARAMS:
    //PATH TO TRAINING DATA
    //NUM OF ITERATIONS
    val th = svm.computeParameters("resources/trainSmall/zips", 50)
    println("classifying...")
    //PARAM:
    //PATH TO VALIDATION/TEST DATA
    val res = svm.classify("resources/validationSmall/zips")
    
    println("ID\tCODES")
     var t = System.currentTimeMillis()
     //PARAM:
     //PATH TO FILE WHERE RESULTS ARE STORED
     var f = new File("resources/FoundTopicLabels/resultSVM"+t)
     f.createNewFile()
     val pw = new PrintWriter("resources/FoundTopicLabels/resultSVM"+t)
     for(t <- res){
       //t: (ID, List of Codes) 
       println(t._1 + " " + t._2.mkString("\t"))
       pw.println(t._1 + " " + t._2.mkString("\t"))
     }
     pw.flush()
     pw.close()
  }
}