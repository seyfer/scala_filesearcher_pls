package fileSearcher

import java.io.File

import scala.util.control.NonFatal

trait IOObject {
  val file: File
  val name = file.getName()
  val fullName = try file.getAbsolutePath() catch {
    case NonFatal(_) => name
  }
}

case class FileObject(file: File) extends IOObject

case class DirectoryObject(file: File) extends IOObject {
  def children() =
    try
      file.listFiles().toList map (file => FileConverter convertToIOOject file)
    catch {
      case _: NullPointerException => List()
    }
}