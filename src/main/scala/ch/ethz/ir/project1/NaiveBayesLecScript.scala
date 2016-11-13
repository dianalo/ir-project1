package ch.ethz.ir.project1

import java.io.PrintWriter
import java.util.Calendar
import java.io.File


//app to compute the Naive Bayes model parameters for a set of documents
//and then classify another set of documents
//parameters needed to run are annotated in the source code below

object NaiveBayesLecScript extends App {
   override def main(args: Array[String]) {
     val config = new Config
     
     //initialize arrays where probabilities are later stored
     var Pcat: Array[Double] = new Array[Double](config.codes.size)
     var logPwc: Array[Map[String, Double]] = new Array[Map[String, Double]](config.codes.size)
     
     //PARAM 2:
     //THRESHOLD FOR CLASSIFICATION
     var nB = new NaiveBayesLec(config, 0.3, Pcat, logPwc)
     println("computing probabilities...")
     //PARAM:
     //PATH TO TRAINING DATA
     nB.computeProbabilities("resources/trainSmall/zips")
     println("done.")
     println("classifying...")
     //PARAM:
     //PATH TO VALIDATION/TEST DATA
     val l = nB.classify("resources/validationSmall/zips")
     println("done")
     
     println("ID\tCODES")
     var t = System.currentTimeMillis()
     //PARAM:
     //PATH TO FILE WHERE RESULTS SHOULD BE STORED
     var f = new File("resources/FoundTopicLabels/resultNB"+t)
     f.createNewFile()
     val pw = new PrintWriter("resources/FoundTopicLabels/resultNB"+t)
     for(t <- l){
       println(t._1 + "\t" + t._2.mkString("\t"))
       pw.println(t._1 + "\t" + t._2.mkString("\t"))
     }
     pw.flush()
     pw.close()
   }
}