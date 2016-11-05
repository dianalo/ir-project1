package ch.ethz.ir.project1

import ch.ethz.dal.tinyir.io._
import java.io.File

object test extends App {
  override def main(args: Array[String]) {
    val trainingFolder = "resources/validation/validation_zip/0"
    val reuters = new ReutersRCVStream(trainingFolder)
    println("Reading from path = " + trainingFolder)
    println("Number of documents = " + reuters.length)
    
    
    // get classification result
    var rList: List[List[String]] = List()
    val files = getListOfFiles("resources/FoundTopicLabels")
    // need to modify result files' paths
    val fileLines = io.Source.fromFile(files(0).toString()).getLines.toList    
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
          val numOfSame = item.zip(doc.codes.toList).filter(x => x._1 == x._2).size
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