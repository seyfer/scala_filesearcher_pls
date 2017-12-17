package fileSearcher

class Benchmarker {
  /**
    *
    * @param page
    * @param pageSize
    * @param totalItems
    * @return
    */
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

  def getListForRange(list: List[(Int, IOObject, Double)], start: Int, end: Int): List[(Int, IOObject, Double)] = {
    return list.dropWhile(_._1 < start)
      .takeWhile(ioGlob => ioGlob._1 >= start && ioGlob._1 <= end)
  }

  def benchmarkGetContent(
                           matcher: Matcher,
                           dataFilter: String,
                           matchedFiles: List[IOObject]
                         ): List[(IOObject, Some[Int])] = {
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
        val currRunList = getListForRange(
          matchedFilesWithIdAndFileSize,
          currPageData._1,
          currPageData._2
        )

        //parallel
        val listToFilter = currRunList.par
        val currentRunStartTime = System.nanoTime()

        val result = matcher.getContentFilterResult(listToFilter, dataFilter)

        val currentRunEndTime = System.nanoTime
        //reduce
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
}

object Benchmarker {
  def apply() = new Benchmarker()
}
