import zio.*
import zio.stream.*

import scala.collection.mutable

object Day7 extends ZIOAppDefault:

  val diskSpace = 70000000
  val neededSpace = 30000000

  enum Entry:
    case Directory(dname: String, entries: mutable.ListBuffer[Entry])
    case File(fname: String, fsize: Int)

    def name: String = this match
      case Directory(dname, entries) => dname
      case File(fname, fsize)        => fname

    def size: Int = this match
      case Directory(_, entries) => entries.map(_.size).sum
      case File(_, fsize)        => fsize

    def isDirectory = this match
      case Directory(_, _) => true
      case File(_, _)      => false

    def findDirectories: List[Directory] = this match
      case dir @ Directory(_, entries) =>
        dir :: entries.toList.flatMap(_.findDirectories)
      case _ => List.empty

  enum Line:
    case Cd(dname: String)
    case Ls
    case Directory(dname: String)
    case File(fname: String, fsize: Int)

  object Line:
    def parse(line: String): Line = line match
      case s"$$ cd $dname"  => Cd(dname)
      case s"$$ ls"         => Ls
      case s"dir $dname"    => Directory(dname)
      case s"$fsize $fname" => File(fname, fsize.toInt)

  class FileSystem:
    val root: Entry.Directory = Entry.Directory("/", mutable.ListBuffer.empty)
    val currentPath: mutable.ListBuffer[Entry.Directory] =
      mutable.ListBuffer.empty

    def cd(target: String): UIO[Unit] =
      ZIO.succeed {
        target match
          case "/"  => currentPath += root
          case ".." => currentPath.dropRightInPlace(1)
          case x =>
            val targetDir =
              currentPath.last.entries
                .find(_.name == target)
                .getOrElse(root)
                .asInstanceOf[Entry.Directory]
            currentPath += targetDir
      }

    def ls(entry: Entry): UIO[Unit] =
      ZIO.succeed {
        currentPath.last.entries += entry
      }

    def findDirectories(upperSize: Int): List[Entry.Directory] =
      root.findDirectories.filter(_.size < upperSize)

    def findDeletionTarget: Entry.Directory =
      val unusedSpace = diskSpace - root.size
      root.findDirectories
        .sortBy(_.size)
        .filter(_.size + unusedSpace >= neededSpace)
        .head

  def execute(fs: FileSystem)(line: Line): UIO[Unit] =
    line match
      case Line.Cd(dname) => fs.cd(dname)
      case Line.Ls        => ZIO.unit
      case Line.Directory(dname) =>
        fs.ls(Entry.Directory(dname, mutable.ListBuffer.empty))
      case Line.File(fname, fsize) => fs.ls(Entry.File(fname, fsize))

  val inputStream =
    ZStream
      .fromFileName("data/input7.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)

  def part[R, E](
      is: ZStream[R, E, String],
      result: FileSystem => Int
  ): ZIO[R, E, Int] =
    for
      fs <- ZIO.succeed(new FileSystem)
      r <- is
        .map(Line.parse)
        .mapZIO(execute(fs))
        .runDrain
    yield result(fs)

  def part1[R, E](
      is: ZStream[R, E, String]
  ): ZIO[R, E, Int] =
    part(is, _.findDirectories(100000).map(_.size).sum)

  def part2[R, E](
      is: ZStream[R, E, String]
  ): ZIO[R, E, Int] =
    part(is, _.findDeletionTarget.size)

  val run = part1(inputStream).debug *> part2(inputStream).debug
