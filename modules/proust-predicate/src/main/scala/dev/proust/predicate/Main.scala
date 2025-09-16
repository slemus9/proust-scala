package dev.proust.predicate

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import dev.proust.predicate.parser.ProgramParser
import dev.proust.predicate.resources.AppResources
import fs2.io.file.Files
import fs2.io.file.Path

object Main extends IOApp {

  private val files = Files[IO]

  override def run(args: List[String]): IO[ExitCode] =
    checkProgram(args).as(ExitCode.Success)

  private def checkProgram(args: List[String]): IO[Unit] =
    for
      app         <- AppResources.make[IO]
      path        <- parseFilePath(args)
      fileContent <- readFile(path)
      program     <- ProgramParser.parseProgram(fileContent).liftTo[IO]
      _           <- app.typeChecker.checkProgram(program)
    yield ()

  private def parseFilePath(args: List[String]): IO[Path] =
    args.headOption
      .liftTo[IO](RuntimeException("Must receive a file path as an argument"))
      .map(Path.apply)

  private def readFile(path: Path): IO[String] =
    files.readUtf8(path).compile.string
}
