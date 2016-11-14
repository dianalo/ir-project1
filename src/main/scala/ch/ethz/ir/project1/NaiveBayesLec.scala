package ch.ethz.ir.project1
import java.io.PrintWriter
import java.util.Calendar
import ch.ethz.dal.tinyir.io._

import breeze.linalg._
import scala.math._

class NaiveBayesLec (val config: Config, 
                      var threshold:Double, var Pcat: Array[Double], var logPwc: Array[Map[String, Double]]){
  
  //encoding of codes in vector: region, topic, industry
  
  def computeProbabilities(trainingDataFolder: String) = {
       var t = System.currentTimeMillis()
       val pw = new PrintWriter("resources/values/prob_values"+t)
       // set threshold to a big value first
       threshold = 1
       
       
        /***********************************
        * slow version for the whole dataset
        ************************************/
//       println("Slow training is running")
//        for( i <- 0 to 49){
//          if(i != 38){
//            val trainingFolder = trainingDataFolder + "/" + i
//            println(trainingFolder)
//            val reuters = new ReutersRCVStream(trainingFolder)
//            println("length of reuters " + reuters.length)
//            val stream = reuters.stream
//            val vocabSize = stream.flatMap(_.tokens).distinct.length
//            val codes = config.codes
//            for(i <- 0 to codes.size-1){
//              val cat = codes(i)
//              val v = log(stream.filter(_.codes(cat)).length /stream.length.toDouble)
//              Pcat(i) = v
//              val tks = stream.filter(_.codes(cat)).flatMap(_.tokens)
//              val denominator = tks.length.toDouble + vocabSize
//              val PwcSparseNumerator = tks.groupBy(identity).mapValues(l=>l.length+1)
//              logPwc(i) = PwcSparseNumerator.mapValues { v => log(v/denominator) }+("_df" -> log(1.0/denominator)) //default value
//              println((i.toDouble/codes.size.toDouble)*100 + "% computed")
//            }
//          }
//        }
      
     /********************************************
      * fast version using train_t to test code
      ********************************************/
       println("fast training is running")
      // quicker path
      val quick_path = "resources/train_t/zips"
      val reuters = new ReutersRCVStream(quick_path)
      val stream = reuters.stream
      val vocabSize = stream.flatMap(_.tokens).distinct.length
      val codes = config.codes
      for(i <- 0 to codes.size-1){
        val cat = codes(i)
        val v = log(stream.filter(_.codes(cat)).length /stream.length.toDouble)
        Pcat(i) = v
        val tks = stream.filter(_.codes(cat)).flatMap(_.tokens)
        val denominator = tks.length.toDouble + vocabSize
        val PwcSparseNumerator = tks.groupBy(identity).mapValues(l=>l.length+1)
        logPwc(i) = PwcSparseNumerator.mapValues { v => log(v/denominator) }+("_df" -> log(1.0/denominator)) //default value
        println((i.toDouble/codes.size.toDouble)*100 + "% computed")
      }
       
//     // get threshold (average)
//     println("reuter size is " + stream.size)
//     // for each file in the stream
//     for (doc <- stream){
//      var this_threshold = 1.0
//      val tks = doc.tokens
//      val tfs = tks.groupBy(identity).mapValues(l=>l.length)
//      for(label <- doc.codes){
//        var sum = 0.0
//        val i = config.codeDictionnary(label)
//        for(word <- tfs.keys){
//          sum += tfs(word)*logPwc(i).getOrElse(word, logPwc(i)("_df"))
//        }
//        sum += Pcat(i)
//        this_threshold = scala.math.min (this_threshold, sum)
//      }
//      threshold += this_threshold
//     }
//     threshold /= stream.size
//     println("threshold is " + threshold)
       
     /**************************
      * fast version finished *
      **************************/
       
       pw.println(Pcat.deep.mkString("\n"))
       
       pw.println(logPwc.deep.mkString("\n"))
     pw.flush();
     pw.close();
  }
  
  def classify(dataFolder: String) : List[List[String]] = {
    var ls: List[List[String]] = List()
    
    /***********************************
    * slow version for the whole dataset
    ************************************/
//    println("slow classification is running")
//    for( i <- 0 to 9){
//      val dFolder = dataFolder + "/" + i
//      val stream = new ReutersRCVStream(dFolder).stream
//      val codes = config.codes
//      var topicScores: Array[Double] = new Array[Double](codes.size)
//      var up_threshold = -10000000.0
//      var j=0
//      for (doc <- stream){
//        up_threshold = -10000000.0
//        val tks = doc.tokens
//        val tfs = tks.groupBy(identity).mapValues(l=>l.length)
//        for(i <- 0 to codes.size-1){
//          var sum = 0.0
//          for(word <- tfs.keys){
//            sum += tfs(word)*logPwc(i).getOrElse(word, logPwc(i)("_df"))
//          }
//          sum += Pcat(i)
//          topicScores(i) = sum
//          up_threshold = scala.math.max(sum, up_threshold)
//        }
//      
//        // test method
//        val temp = topicScores.toList.sortWith (_ > _).take(100)
//        println("temp is " + temp(99))
//        threshold = (up_threshold + temp(9)) / 2      
//        if(threshold < -1000000)
//          threshold = (up_threshold + temp(9)) / 2      
//        println("threshold is " + threshold)
//        println("up threshold is " + up_threshold)
//        
//        //get single best labels... maybe multiple labels via thresholding
//        var l = config.invCodeDictionnaryInList(topicScores.zipWithIndex.filter{_._1 >= threshold})
//        
//        ls = l::ls
//        
//        j+=1
//        println((j.toDouble/stream.length.toDouble)*100 + "% classified")
//      }
//    }
    
    
    /********************************************
    * fast version using validation_zip/0 to test code
    ********************************************/
    println("fast classification is running")
    val dFolder = dataFolder + "/" + 0
    val stream = new ReutersRCVStream(dFolder).stream
    val codes = config.codes
    var topicScores: Array[Double] = new Array[Double](codes.size)
    
    // for threshold (test method)
    var up_threshold = -10000000.0
    var j = 0
    for (doc <- stream){
        up_threshold = -10000000.0
      val tks = doc.tokens
      val tfs = tks.groupBy(identity).mapValues(l=>l.length)
      for(i <- 0 to codes.size-1){
        var sum = 0.0
        for(word <- tfs.keys){
          sum += tfs(word)*logPwc(i).getOrElse(word, logPwc(i)("_df"))
        }
        sum += Pcat(i)
        topicScores(i) = sum
        up_threshold = scala.math.max(sum, up_threshold)
      }
     
      // test method
      def average[T]( ts: Iterable[T] )( implicit num: Numeric[T] ) = {
        num.toDouble( ts.sum ) / ts.size
      }
      val temp = topicScores.toList.sortWith (_ > _).take(30)
      threshold = (average(temp.take(3).toList.filter(_ > -1000000)) + average(temp.drop(27).take(3).toList.filter(_ > -1000000))) / 2
//      println("temp is " + temp(99))
//      threshold = (up_threshold + temp(99)) / 2      
//      if(threshold < -1000000)
//        threshold = (up_threshold + temp(49)) / 2      
      println("threshold is " + threshold)
//      println("up threshold is " + up_threshold)
      
      //get single best labels... maybe multiple labels via thresholding
      var l = config.invCodeDictionnaryInList(topicScores.zipWithIndex.filter{_._1 >= threshold})
      
      //filter region
      val region = config.invCodeDictionnary(topicScores.slice(0, config.nRegionCodes-1).zipWithIndex.maxBy(_._1)._2)
      //filter topic
      val topic = config.invCodeDictionnary(topicScores.slice(config.nRegionCodes, config.nRegionCodes+config.nTopicCodes-1).zipWithIndex.maxBy(_._1)._2)
      //filter industry
      val industry = config.invCodeDictionnary(topicScores.slice(config.nRegionCodes+config.nTopicCodes, codes.size-1).zipWithIndex.maxBy(_._1)._2)
      
      l = doc.ID.toString() :: l
      ls = l::ls
      
      j+=1
      println((j.toDouble/stream.length.toDouble)*100 + "% classified")
    }
          
          
    return ls
  }
}