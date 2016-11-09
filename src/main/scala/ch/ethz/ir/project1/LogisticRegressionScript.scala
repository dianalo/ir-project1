package ch.ethz.ir.project1

import ch.ethz.dal.tinyir.lectures._

import java.io.File
import java.io.PrintWriter

object LogisticRegressionScript extends App {
  override def main(args: Array[String]){
    val config = new Config
    
    var theta: Array[SMap] = new Array[SMap](config.codes.length)
    for(i <- 0 to config.codes.length-1){
      theta(i) = new SMap(Map[String, Double]())
    }
    
    var lR = new LogisticRegression(config, theta, 0.3)
    
    println("computing parameters...")
    //params: folder, iterations, learningRate
    theta = lR.computeParameters("resources/trainSmall/zips", 100, 1/config.nDocs)
    println("classifying...")
    val res = lR.classify("resources/validationSmall/zips")
    
    println("ID\tCODES")
     var t = System.currentTimeMillis()
     var f = new File("resources/FoundTopicLabels/resultLR"+t)
     f.createNewFile()
     val pw = new PrintWriter("resources/FoundTopicLabels/resultLR"+t)
     for(t <- res){
       //t: (ID, List of Codes) 
       println(t._1 + " " + t._2.mkString("\t"))
       pw.println(t._1 + " " + t._2.mkString("\t"))
     }
      //pw.println("used model:")
      //pw.println(theta.mkString(", "))
     pw.flush()
     pw.close()
  }
}