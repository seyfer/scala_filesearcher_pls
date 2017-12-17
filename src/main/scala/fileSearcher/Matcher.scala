package fileSearcher

import java.io.File

import scala.annotation.tailrec
import scala.collection.parallel.ParSeq

/**
  * main entry point
  *
  * @param filter
  * @param rootLocation
  * @param checkSubFolders
  * @param contentFilter
  */
class Matcher(
               filter: String,
               val rootLocation: String = new File(".").getCanonicalPath(),
               checkSubFolders: Boolean = false,
               contentFilter: Option[String] = None
             ) {

  val rootIOObject = FileConverter.convertToIOOject(new File(rootLocation))

  def getContentFilterResult(
                              listToFilter: ParSeq[(Int, IOObject, Double)],
                              dataFilter: String
                            ): ParSeq[(IOObject, Some[Int])] =
    listToFilter
      .map(iOObject => (
        iOObject._2,
        Some(FilterChecker(dataFilter).findMatchedContentCount(iOObject._2.file)))
      )
      .filter(matchTuple => matchTuple._2.get > 0)

  /**
    *
    * @return list of filenames
    */
  def execute(): List[(String, Option[Int])] = {

    def contentMatch(dataFilter: String, matchedFiles: List[IOObject]): List[(IOObject, Some[Int])] = {
      var result = Benchmarker().benchmarkGetContent(this, dataFilter, matchedFiles)
      return result
    }

    @tailrec
    def recursiveMatch(files: List[IOObject], currentList: List[FileObject]): List[FileObject] =
      files match {
        case List() => currentList
        case iOObject :: rest =>
          iOObject match {
            case file:
              FileObject
              if FilterChecker(filter).matches(file.name) =>
              recursiveMatch(rest, file :: currentList)
            case directory:
              DirectoryObject =>
              recursiveMatch(rest ::: directory.children(), currentList)
            case _ => recursiveMatch(rest, currentList)
          }
      }

    val matchedFiles = rootIOObject match {
      case file: FileObject if FilterChecker(filter) matches file.name => List(file)
      case directory: DirectoryObject =>
        if (checkSubFolders) recursiveMatch(directory.children(), List())
        else FilterChecker(filter).findMatchedFiles(directory.children())
      case _ => List()
    }

    val contentFilteredFiles = contentFilter match {
      case Some(dataFilter) => contentMatch(dataFilter, matchedFiles)
      case None => matchedFiles.map(iOObject => (iOObject, None))
      case null => null
    }

    if (contentFilteredFiles == null) {
      return null
    }

    return contentFilteredFiles.map {
      case (iOObject, count) => (iOObject.fullName, count)
    }
  }
}

object Matcher {
  def apply(
             filter: String,
             rootLocation: String,
             checkSubFolders: Boolean,
             contentFilter: Option[String]
           ): Matcher = new Matcher(filter, rootLocation, checkSubFolders, contentFilter)
}