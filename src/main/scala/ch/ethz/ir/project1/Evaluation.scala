package ch.ethz.ir.project1

object Evaluation extends App {
  override def main(args: Array[String]){
    //val config = new Config()
    
    val nValDocs = 73
    
    val docPath = "resources/validationSmall/zips"
    val resPath = "resources/FoundTopicLabels/resultNB_THRESHOLD_0.6"
    
    var p = new Array[Double](nValDocs)
    var r = new Array[Double](nValDocs)
    var f1 = new Array[Double](nValDocs)
    
    var f1avg = 0.0
    
    var measurement = new PrecRec(resPath, docPath, p, r, f1, f1avg)
    measurement.evaluate()
       
    //println("Doc with best precision: " + p.zipWithIndex.maxBy(_._1))
    println("Best precision: " + p.max)
    println("Worst precision: " + p.min)
    
    println("Best recall: " + r.max)
    println("Worst recall: " + r.min)
    
    println("Best F1 score: " + f1.max)
    println("Worst F1 score: " + f1.min)
    
    println("Average F1 Score: " + f1avg)
  }
}