package ch.ethz.ir.project1

import scala.io.Source

/**
 * @author ${user.name}
 */
  object App {

    def main(args: Array[String]) {
      var helper = new Helper()
      
      println(helper.allWordsDic(10).length)
    }
    

  }

