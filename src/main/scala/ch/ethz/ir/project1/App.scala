package ch.ethz.ir.project1

import scala.io.Source

/**
 * @author ${user.name}
 */
  object App {

    def main(args: Array[String]) {
      //parse documents in train set
      val Helper = new Helper()
      val Config = new Config()
      
      var codeMap = Helper.makeCodeMap
       
      //compute term frequency map
      var fList = Helper.getListOfFiles("resources/trainSmall").map(_.getPath);
      
    }
    

  }

