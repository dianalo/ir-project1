package ch.ethz.ir.project1

import java.io.File
import scala.io.Source
import scala.collection.mutable._
import ch.ethz.dal.tinyir.io.ReutersRCVStream


class Helper {
      def getListOfFiles(dir: String): List[File] = {
      val d = new File(dir)
      if (d.exists && d.isDirectory()) {
        d.listFiles.filter(_.isFile).toList
      } else {
        List[File]()
      }
    }
      
      def makeCodeMap : Map[String, Any] = {
        //create map with all codes
      var codesMap = new LinkedHashMap[String, Any]()
      
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
      
      def allWordsDic(minLength: Int) : List[(String, Int)] = {
        var str = new ReutersRCVStream("resources/train/zips").stream
        var wSet = new HashSet[String]()
        for(d <- str){
          val newWords = d.tokens.distinct.filter(_.length >= minLength).filter(a => wSet.contains(a))
          newWords.map { w => wSet.add(w)}
        }
        val l = wSet.toList.sorted.zipWithIndex
        
        return l
      }
}