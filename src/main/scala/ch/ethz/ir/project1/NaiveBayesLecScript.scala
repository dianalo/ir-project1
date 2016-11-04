package ch.ethz.ir.project1

import java.io.PrintWriter
import java.util.Calendar
import java.io.File

object NaiveBayesLecScript extends App {
   override def main(args: Array[String]) {
     val config = new Config
     
     var Pcat: Array[Double] = new Array[Double](config.codes.size)
     var logPwc: Array[Map[String, Double]] = new Array[Map[String, Double]](config.codes.size)
     
     var nB = new NaiveBayesLec(config, 0.75, Pcat, logPwc)
     println("computing probabilities...")
     nB.computeProbabilities("resources/train/train_zip")
     println("done.")
     println("classifying...")
     val l = nB.classify("resources/validation/validation_zip")
     println("done")
     
     println("REGION\tTOPIC\tINDUSTRY")
     var t = System.currentTimeMillis()
     var f = new File("resources/FoundTopicLabels/result"+t)
     f.createNewFile()
     val pw = new PrintWriter("resources/FoundTopicLabels/result"+t)
     for(t <- l){
       println(t.mkString("\t"))
       pw.println(t.mkString("\t"))
     }
     pw.flush();
     pw.close();
   }
}