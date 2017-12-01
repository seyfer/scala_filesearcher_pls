package fileSearcher

import java.io._

object SearchResultWriter {
  def writeToFile(
                   filePath: String,
                   searchResults: List[(String, Option[Int])]
                 ): Unit = {

    val fileWriter = new FileWriter(filePath)
    val printWriter = new PrintWriter(fileWriter)

    try
        for ((fileName, countOption) <- searchResults)
          printWriter.println(getString(fileName, countOption))
    finally {
      printWriter.close()
      fileWriter.close()
    }
  }

  def writeToConsole(searchResults: List[(String, Option[Int])]): Unit = {
    for ((fileName, countOption) <- searchResults)
      println(getString(fileName, countOption))
  }

  private def getString(fileName: String, countOption: Option[Int]) = {
    countOption match {
      case Some(count) => s"$fileName -> $count"
      case None => s"$fileName"
    }
  }
}