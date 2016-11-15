package ch.ethz.ir.project1

import ch.ethz.dal.tinyir.io._
import java.io.File

class PrecRec (resultPath: String, validationPath: String, var p: Array[Double], var r: Array[Double], var f1: Array[Double], var avgF1: Double) {
  
  
  def evaluate() : Unit = {
    
    // get validation set
    // val trainingFolder = "resources/validation/validation_zip/0"
    var valDocs = new ReutersRCVStream(validationPath).stream.toList
    valDocs.sortBy(_.ID)
    //println(valDocs.head.ID)
    
    // get classification result
    var rList: List[List[String]] = List()
    val fileLines = io.Source.fromFile(resultPath).getLines.toList
    for(token <- fileLines)
    {
      val l = token.split("[ \t]+").toList
      rList = l::rList
    }
    
    // maybe sort the lists to make it faster?
    rList = rList.reverse
    //println(rList.head.head)
    
    // evaluation parameters
    var labelsAssigned = 0
    var realPositives = 0
    var tp = 0
    
    // for all files in the validation set
    for(i <- 0 to valDocs.size-1){
      val valDoc = valDocs(i)
      realPositives = valDoc.codes.size
      // for all items in the result list
      val rTup = rList(i)
      
      if(rTup.head.toInt != valDoc.ID.toInt){
        println(rTup.head + "\t" + valDoc.ID)
        println("Different contents in List! STOPPED.")
        return
      }
      
      val numOfSame = rTup.filter(valDoc.codes.contains(_)).size
      tp = numOfSame
      labelsAssigned = rTup.length-1
     
      if(labelsAssigned == 0){
        p(i) = 0.0
      }
      else{
        p(i) = tp.toDouble / labelsAssigned.toDouble
      }
      if(realPositives == 0){
        r(i) = 0.0
      }
      else{
        r(i) = tp.toDouble / realPositives.toDouble
      }
      if(p(i)+r(i) == 0.0){
        f1(i) = 0.0
      }
      else{
        f1(i) = 2*p(i)*r(i)/(p(i)+r(i))
      }
      
    }
    

    avgF1 = f1.sum.toDouble / f1.size.toDouble 
    
    println("Best precision: " + p.max)
    println("Worst precision: " + p.min)
    
    println("Best recall: " + r.max)
    println("Worst recall: " + r.min)
    
    println("Best F1 score: " + f1.max)
    println("Worst F1 score: " + f1.min)
    
    println("Average F1 Score: " + avgF1)
  }
     
}