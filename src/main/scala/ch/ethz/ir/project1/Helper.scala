package ch.ethz.ir.project1

import java.io.File
import scala.io.Source


class Helper {
      def getListOfFiles(dir: String): List[File] = {
      val d = new File(dir)
      if (d.exists && d.isDirectory()) {
        d.listFiles.filter(_.isFile).toList
      } else {
        List[File]()
      }
    }
      
      def makeCodeMap : collection.mutable.Map[String, List[Any]] = {
        //create map with all codes
      var codesMap = collection.mutable.LinkedHashMap[String, List[Any]]()
      
      //add country codes
      for(line <- Source.fromFile("resources/codes/region_codes.txt").getLines()){
        val s = line.split("[\"\'\t\n\r\f]+").head
        if(!s.equals(";")){
          codesMap += (s -> List())
        }
      }
      
      //add topic codes
      for(line <- Source.fromFile("resources/codes/topic_codes.txt").getLines()){
        val s = line.split("[\"\'\t\n\r\f]+").head
        if(!s.equals(";")){
          codesMap += (s -> List())
        }
      }
      
      //add industry codes
      for(line <- Source.fromFile("resources/codes/industry_codes.txt").getLines()){
        val s = line.split("[\"\'\t\n\r\f]+").head
        if(!s.equals(";")){
          codesMap += (s -> List())
        }
      }
      
      return codesMap
      }
      
      
      
}