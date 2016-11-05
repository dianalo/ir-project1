package ch.ethz.ir.project1

import ch.ethz.dal.tinyir.io._
import java.io.File

class PrecRec (var p: Double, var r: Double) {
  def evaluate (resultPath: String, validationPath: String) = {
    // get validation set
//    val trainingFolder = "resources/validation/validation_zip/0"
    
    val reuters = new ReutersRCVStream(validationPath)
    println("Reading from path = " + validationPath)
    println("Number of documents = " + reuters.length)
    
    
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
    var relItemRetrieved = 0
    var itemRetrieved = 0
    var relItem = 0
    
    // for all files in the validation set
    for(doc <- reuters.stream){
      relItem += doc.codes.size
      println("codes for file " + doc.name + " is " + doc.codes);
      // for all items in the result list
      for(item <- rList){
        if(item(0) == doc.name){
          val numOfSame = item.size - item.filterNot(doc.codes.toSet).size
          // for debug
          println(numOfSame)
          relItemRetrieved += numOfSame
          itemRetrieved += item.length-1
        }
      }
    }
    println("relevant item retrieved " + relItemRetrieved)
    println("item retrieved " + itemRetrieved)
    println("relevant item in collection " + relItem)
    
    p = relItemRetrieved.toDouble / itemRetrieved.toDouble
    r = relItemRetrieved.toDouble / relItem.toDouble
    
    println("Precision is " + p)
    println("Recall is " + r)

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