package fileSearcher

import java.io.File

import scala.annotation.tailrec

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

  /**
    *
    * @return list of filenames
    */
  def execute(): List[(String, Option[Int])] = {

    def getPagingInfo(page: Int, pageSize: Int, totalItems: Int): (Int, Int, Int) = {
      val pages = 1 to totalItems by pageSize

      if (pages.isEmpty) {
        return null
      }

      val from = pages(page - 1)
      val to = from + pageSize - 1 min totalItems
      val totalPages = pages.size
      return (from, to, totalPages)
    }

    def getListForRange(list: List[(Int, IOObject, Double)],
                        start: Int, end: Int) =
      list.dropWhile(_._1 < start)
        .takeWhile(ioGlob => ioGlob._1 >= start && ioGlob._1 <= end)

    def contentMatch(dataFilter: String, matchedFiles: List[IOObject]): List[(IOObject, Some[Int])] = {
      var id = 0
      val matchedFilesWithIdAndFileSize = matchedFiles.map(x => {
        id = id + 1
        (id, x, x.fileSize)
      })

      val pageData = getPagingInfo(1, 10000, matchedFiles.length)
      if (pageData == null) {
        return null
      }

      println("starting")
      var totalMb = 0D
      val beginTime = System.nanoTime()
      val totalPages = pageData._3
      val results = for (currPage <- 1 to totalPages)
        yield {
          val currPageData = getPagingInfo(currPage, 10000, matchedFiles.length)
          val currRunList = getListForRange(matchedFilesWithIdAndFileSize,
            currPageData._1, currPageData._2)

          //parallel
          val listToFilter = currRunList.par
          val currentRunStartTime = System.nanoTime()

          val result = listToFilter
            .map(iOObject => (iOObject._2,
              Some(FilterChecker(dataFilter).findMatchedContentCount(iOObject._2.file))))
            .filter(matchTuple => matchTuple._2.get > 0)

          val currentRunEndTime = System.nanoTime
          val currentRunMb = listToFilter.foldLeft(0D)((accum, ioGlob) =>
            accum.toDouble + ioGlob._3)
          totalMb = totalMb + currentRunMb
          println(s"page: $currPage; "
            + s"Total Running Time: ${(currentRunEndTime - beginTime) * 1e-9}; "
            + s"Total MB: $totalMb; "
            + s"Current Run Time: ${(currentRunEndTime - currentRunStartTime) * 1e-9}; "
            + s"Current Run MB: $currentRunMb")
          result
        }
      val finalTime = System.nanoTime()
      println(s"Total Running Time: ${(finalTime - beginTime) * 1e-9}; "
        + s"Total MB: $totalMb")

      return results.toList.flatten
    }

    @tailrec
    def recursiveMatch(files: List[IOObject], currentList: List[FileObject]): List[FileObject] =
      files match {
        case List() => currentList
        case iOObject :: rest =>
          iOObject match {
            case file: FileObject if FilterChecker(filter) matches file.name =>
              recursiveMatch(rest, file :: currentList)
            case directory: DirectoryObject =>
              recursiveMatch(rest ::: directory.children(), currentList)
            case _ => recursiveMatch(rest, currentList)
          }
      }

    val matchedFiles = rootIOObject match {
      case file: FileObject if FilterChecker(filter) matches file.name => List(file)
      case directory: DirectoryObject =>
        if (checkSubFolders) recursiveMatch(directory.children(), List())
        else FilterChecker(filter) findMatchedFiles directory.children()
      case _ => List()
    }

    val contentFilteredFiles = contentFilter match {
      case Some(dataFilter) => contentMatch(dataFilter, matchedFiles)
      case None => matchedFiles.map(iOObject => (iOObject, None))
      case null => null
    }

    if (contentFilteredFiles == null) {
      return null;
    }

    return contentFilteredFiles.map {
      case (iOObject, count) => (iOObject.fullName, count)
    }
  }
}