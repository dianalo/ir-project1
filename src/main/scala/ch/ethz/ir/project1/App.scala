package ch.ethz.ir.project1

import ch.ethz.dal.tinyir.processing.ReutersRCVParse
import ch.ethz.dal.tinyir.io.ReutersRCVStream

import ch.ethz.dal.tinyir.io.DocStream

import java.io.File;

/**
 * @author ${user.name}
 */
object App {
  
  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory()) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
}
  
  def main(args : Array[String]) {
    //parse documents in train set
    var fList = getListOfFiles("resources/train").map(_.getPath);
    for(fName <- fList){
       val parse = new ReutersRCVParse(DocStream.getStream(fName));
       println(parse.title)
    }
  }
  
}
