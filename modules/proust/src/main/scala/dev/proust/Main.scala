package dev.proust

import cats.effect.IO
import cats.effect.IOApp

object Main extends IOApp.Simple:

  override def run: IO[Unit] =
    IO.println("Proust")
