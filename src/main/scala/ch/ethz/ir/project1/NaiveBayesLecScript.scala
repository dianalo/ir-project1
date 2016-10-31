package ch.ethz.ir.project1

object NaiveBayesLecScript extends App {
   override def main(args: Array[String]) {
     val config = new Config
     
     var Pcat: Array[Double] = new Array[Double](config.codes.size)
     var logPwc: Array[Map[String, Double]] = new Array[Map[String, Double]](config.codes.size)
     
     
     //folder: "/Users/loris/git/ir-project1/src/ch/ethz/ir/project1/resources/trainSmall"
     var nB = new NaiveBayesLec(config, 0.75, Pcat, logPwc)
     nB.computeProbabilities("/Users/loris/git/ir-project1/resources/trainSmall")
     val l = nB.classify("resources/testSmall")
     
     println("REGION\tTOPIC\tINDUSTRY")
     for(t <- l){
       println(t.mkString("\t"))
     }
   }
}