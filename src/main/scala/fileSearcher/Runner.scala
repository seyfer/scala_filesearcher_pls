package fileSearcher

object Runner extends App {

  def convertToBool(boolString: String): Boolean = {
    return List("true", "t").contains(boolString.toLowerCase())
  }

  val matcher = args match {
    case Array(filter) => new Matcher(filter)
    case Array(filter, rootPath) => new Matcher(filter, rootPath)
    case Array(filter, rootPath, checkSubfolders) =>
      new Matcher(filter, rootPath, convertToBool(checkSubfolders))
    case Array(filter, rootPath, checkSubfolders, contentFilter, _*) =>
      new Matcher(filter, rootPath, convertToBool(checkSubfolders), Some(contentFilter))
  }

  val results = matcher.execute()

  if (results == null) {
    println(s"Not Found")
  } else {
    println(s"Found ${results.length} matches:")
    args match {
      case Array(_, _, _, _, outputFilePath, _*) => {
        SearchResultWriter.writeToFile(outputFilePath, results)
        println(s"Results writtent to $outputFilePath")
      }
      case _ => SearchResultWriter.writeToConsole(results)
    }
  }

}
