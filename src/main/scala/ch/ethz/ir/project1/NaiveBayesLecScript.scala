package ch.ethz.ir.project1

import java.io.PrintWriter
import java.util.Calendar
import java.io.File

object NaiveBayesLecScript extends App {
   override def main(args: Array[String]) {
     val config = new Config
     
     var Pcat: Array[Double] = new Array[Double](config.codes.size)
     var logPwc: Array[Map[String, Double]] = new Array[Map[String, Double]](config.codes.size)
     
     var nB = new NaiveBayesLec(config, 0.5, Pcat, logPwc)
     println("computing probabilities...")
     nB.computeProbabilities("resources/trainSmall/zips")
     println("done.")
     println("classifying...")
     val l = nB.classify("resources/validationSmall/zips")
     println("done")
     
     println("ID\tREGION\tTOPIC\tINDUSTRY")
     var t = System.currentTimeMillis()
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