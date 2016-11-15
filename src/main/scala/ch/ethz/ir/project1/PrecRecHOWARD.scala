package ch.ethz.ir.project1

import ch.ethz.dal.tinyir.io._
import java.io.File

class PrecRecHoward (var p: Double, var r: Double) {
  def evaluate (resultPath: String, validationPath: String) : Unit = {
    // get validation set
    // val trainingFolder = "resources/validation/validation_zip/0"
    
    var valDocs = new ReutersRCVStream(validationPath).stream.toList
    valDocs.sortBy(_.ID)
    println(valDocs.head.ID)
    
    // get classification result
    var rList: List[List[String]] = List()
    val fileLines = io.Source.fromFile(resultPath).getLines.toList
    for(token <- fileLines)
    {
      val l = token.split("[ .,;:?!\t\n\r\f]+").toList
      rList = l::rList
    }
    println(rList);
    
    // maybe sort the lists to make it faster?
//    rList = rList.sortWith(_(0) > _(0))
//    println(rList);
//    reuters.stream.sortWith(_(0) > _(0))
    
    // evaluation parameters
    var F1_score_sum : Double = 0
    
    // for all files in the test set
    for(doc <- valDocs){
      // relevant items are all the codes contained by a certain doc
      val relItem = doc.codes.size
      println("codes for file " + doc.name + " is " + doc.codes);
      // for all items in the result list, find the corresponding file
      for(item <- rList){
        // find the corresponding file by itemID
        if(item(0) == doc.name){
          // filter for the same categories
          val relItemRetrieved = item.filter(doc.codes.toSet).size
          // for debug
          println(relItemRetrieved)
          // the number of item retrieved is the list read from the result file minus the first element which is the item ID
          val itemRetrieved = item.size - 1
          println(itemRetrieved)
          if(itemRetrieved == 0){
            p = 0
          } else {
            p = relItemRetrieved.toDouble / itemRetrieved.toDouble
          } 
          r = relItemRetrieved.toDouble / relItem.toDouble
          
          println("precision is " + p)
          println("recall is " + r)
          
          if(p == 0 & r == 0){
            F1_score_sum = 0
          } else {
            F1_score_sum += 2 * p * r / (p + r)  
          }
        }
      }
    }
    var F1_score = F1_score_sum / valDocs.size
    println("F1 score is " + F1_score)

  }
  
  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }    
}