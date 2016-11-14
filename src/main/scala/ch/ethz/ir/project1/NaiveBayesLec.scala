package ch.ethz.ir.project1
import java.io.PrintWriter
import java.util.Calendar
import ch.ethz.dal.tinyir.io._

import scala.math._
import ch.ethz.dal.tinyir.processing.StopWords
import com.github.aztek.porterstemmer.PorterStemmer

/*
 * NaiveBayes Class
 * provides all the functions needed to compute parameters and classify
 * according to NaiveBayes classifier, pretty much the same as on the slides
 * params:
 * config: configuration object
 * threshold: global threshold to classify document
 * Pcat: array where probabilities P(c) are stored
 * Pwc: matrix where probabilites P(w|c) are stored for each class/label
 * */
class NaiveBayesLec (val config: Config, 
                      var threshold:Double, var Pcat: Array[Double], var logPwc: Array[Map[String, Double]]){
  
  
  /*
   * computes probabilites P(c) and P(w|c) for given training data
   * 
   * */ 
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
      //vocabsize could be hardcoded for full training set
      val vocabSize = stream.flatMap(d => StopWords.filterOutSW(d.tokens)).map(PorterStemmer.stem(_)).distinct.length
      val codes = config.codes
      for(i <- 0 to codes.size-1){
        val cat = codes(i)
        val docsInCat = stream.filter(_.codes(cat))
        //log not really needed
        val v:Double = docsInCat.length.toDouble / stream.length.toDouble
        Pcat(i) = v
        val tks = docsInCat.flatMap(d => StopWords.filterOutSW(d.tokens)).map(PorterStemmer.stem(_))
        val denominator: Double = tks.length.toDouble + vocabSize.toDouble
        val PwcSparseNumerator = tks.groupBy(identity).mapValues(l=>l.length+1)
        //log not really needed
        logPwc(i) = PwcSparseNumerator.mapValues { v => v.toDouble/denominator}+("_df" -> 1.0/denominator) //default value
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
  
  /*
   * classifies documents in given folder
   * 
   * returns:
   * list of tuples containing the Document ID and a List of all identified labels
   * the returned list has the same order as the read in documents
   * */
  def classify(dataFolder: String) : List[(Int,List[String])] = {
    var ls: List[(Int,List[String])] = List()
    
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
      
      val assignedTopics = topicScores.zipWithIndex.filter(_._1 > threshold).map(s => config.invCodeDictionnary(s._2))
      val l = (doc.ID, assignedTopics.toList)
      
      ls = l::ls
      
      j+=1
      println((j.toDouble/stream.length.toDouble)*100 + "% classified")
    }
          
    return ls.reverse
  }
}