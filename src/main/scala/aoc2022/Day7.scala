package aoc2022

import zio.*
import zio.stream.*

import scala.collection.mutable
import java.io.File

object Day7 extends ZIOAppDefault:

  val diskSpace = 70000000
  val neededSpace = 30000000

  enum Entry:
    case Directory(
        name: String,
        subdirs: mutable.ListBuffer[Directory],
        files: mutable.ListBuffer[File]
    )
    case File(name: String, fsize: Int)

    def name: String
    def size: Int = this match
      case Directory(_, subdirs, files) =>
        subdirs.map(_.size).sum + files.map(_.size).sum
      case File(_, fsize) => fsize

  object Entry:
    def mkdir(name: String): Directory =
      Directory(name, mutable.ListBuffer.empty, mutable.ListBuffer.empty)

  extension (dir: Entry.Directory)
    def findDirectories: List[Entry.Directory] =
      dir :: dir.subdirs.toList.flatMap(_.findDirectories)

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

  class FileSystem private ():
    val root: Entry.Directory = Entry.mkdir("/")
    val currentPath: mutable.ListBuffer[Entry.Directory] =
      mutable.ListBuffer.empty

    def cd(target: String): UIO[Unit] =
      ZIO.succeed {
        target match
          case "/"  => currentPath += root
          case ".." => currentPath.dropRightInPlace(1)
          case x =>
            val targetDir =
              currentPath.last.subdirs
                .find(_.name == target)
                .getOrElse(root)
            currentPath += targetDir
      }

    def addToCurrentDirectory(newEntry: Entry): UIO[Unit] =
      ZIO.succeed {
        newEntry match
          case d @ Entry.Directory(_, _, _) => currentPath.last.subdirs += d
          case f @ Entry.File(_, _)         => currentPath.last.files += f
      }

    def findDirectories(upperSize: Int): List[Entry.Directory] =
      root.findDirectories.filter(_.size < upperSize)

    def findDeletionTarget: Entry.Directory =
      val unusedSpace = diskSpace - root.size
      root.findDirectories
        .sortBy(_.size)
        .filter(_.size + unusedSpace >= neededSpace)
        .head

  object FileSystem:
    def make: UIO[FileSystem] = ZIO.succeed(new FileSystem())

  def execute(fs: FileSystem)(line: Line): UIO[Unit] =
    line match
      case Line.Cd(dname) => fs.cd(dname)
      case Line.Ls        => ZIO.unit
      case Line.Directory(dname) =>
        fs.addToCurrentDirectory(Entry.mkdir(dname))
      case Line.File(fname, fsize) =>
        fs.addToCurrentDirectory(Entry.File(fname, fsize))

  val inputStream =
    ZStream
      .fromFileName("data/input7.txt")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)

  def part[R, E](
      is: ZStream[R, E, String],
      result: FileSystem => Int
  ): ZIO[R, E, Int] =
    for
      fs <- FileSystem.make
      _ <- is
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
