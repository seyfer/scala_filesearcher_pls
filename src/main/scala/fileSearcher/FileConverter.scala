package fileSearcher

import java.io.File

object FileConverter {
  def convertToIOOject(file: File) =
    if (file.isDirectory())
      DirectoryObject(file)
    else
      FileObject(file)
}