package fileSearcher

import java.io.File

import scala.util.control.NonFatal

class FilterChecker(filter: String) {
  val filterAsRegex = filter.r

  def matches(content: String): Boolean = {
    filterAsRegex.findFirstMatchIn(content) match {
      case Some(_) => true
      case None => false
    }
  }

  def findMatchedFiles(iOObjects: List[IOObject]): List[IOObject] = {
    for (iOObject <- iOObjects
         if (iOObject.isInstanceOf[FileObject])
         if (matches(iOObject.name)))
      yield iOObject
  }

  def findMatchedContentCount(file: File): Int = {
    def getFilterMatchCount(content: String): Int = {
      (filterAsRegex findAllIn content).length
    }

    import scala.io.Source
    try {
      val fileSource = Source.fromFile(file)
      try {
        //reduce
        fileSource.getLines().foldLeft(0)(
          (accumulator, line) => accumulator + getFilterMatchCount(line))
      }
      catch {
        case NonFatal(_) => 0
      }
      finally {
        fileSource.close()
      }
    }
    catch {
      case NonFatal(_) => 0
    }
  }
}

//in order to use it without new
object FilterChecker {
  def apply(filter: String) = new FilterChecker(filter)
}