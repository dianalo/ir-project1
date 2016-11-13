package ch.ethz.ir.project1

import ch.ethz.dal.tinyir.lectures._
import java.io._

object SVMScript extends App {
  override def main(args: Array[String]) = {
    val config = new Config
    
    var theta: Array[SMap] = new Array[SMap](config.codes.length)
    for(i <- 0 to config.codes.length-1){
      theta(i) = new SMap(Map[String, Double]())
    }
    
    var svm = new SVM(config, theta, 1.0)
    
    println("computing parameters...")
    //params: folder, iterations, learningRate
    val th = svm.computeParameters("resources/trainSmall/zips", 50)
    println(th(0).toString())
    println("classifying...")
    val res = svm.classify("resources/validationSmall/zips")
    
    println("ID\tCODES")
     var t = System.currentTimeMillis()
     var f = new File("resources/FoundTopicLabels/resultSVM"+t)
     f.createNewFile()
     val pw = new PrintWriter("resources/FoundTopicLabels/resultSVM"+t)
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