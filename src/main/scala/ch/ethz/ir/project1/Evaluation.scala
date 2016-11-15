package ch.ethz.ir.project1

object Evaluation extends App {
  override def main(args: Array[String]){
    
    val nValDocs = 1000
    
    val docPath = "resources/validationHoward"
    val resPath = "resources/FoundTopicLabels/resultSVMHOWARD.txt"
    
    var p = new Array[Double](nValDocs)
    var r = new Array[Double](nValDocs)
    var f1 = new Array[Double](nValDocs)
    
    var f1avg = 0.0
    
    var measurement = new PrecRec(resPath, docPath, p, r, f1, f1avg)
    measurement.evaluate()
       
  }
}