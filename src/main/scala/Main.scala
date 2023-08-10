object Main {

  import Common._
  import Parser.{Par => P}
  import Parser.Par

  def main(args: Array[String]): Unit = {
    println("Hello world!")

    val fenced: (Char) => Par[String, String, Unit] = f => for {
      _ <- charP(f)
      strLen <- noConsumeP(altP(
        stringP("\\" + f).map(_.length),
        notP(charP(f)).map(_ => 1)
      )).mapErr(_ => ())
      str <- takeStrP(strLen)
      _ <- charP(f)
    } yield str

    val stringLitP: Par[String, String, Unit] = fenced('"')
    val valP = altP(intP)

    val str1 = " [1, 2,] "
  }
}

package Json {
  sealed abstract class Json

  case class Bool(inner: Boolean) extends Json

  case class Number(inner: Int) extends Json
}

package object Common {

  import Helpers._
  import Parser._
  import Parser.Par
  import Parser.ParObj.get

  import scala.util.Try

  def takeStrP(t: Int): Par[String, String, Unit] = {
    Par { i => Right((i.substring(t), i.substring(t)))}
  }

  /**
   * Takes a parser and returns a modified one, which doesn't consume its input
   */
  def noConsumeP[In, Out, Err](p: Par[In, Out, Err]): Par[In, Out, Err] = {
    Par(i => {
      val t = p(i)
      t.map(o => (i, o._2))
    })
  }

  def notP[In, Out, Err](p: Par[In, Out, Err]): Par[In, Unit, Unit] = {
    Par { i =>
      p(i) match {
        case Right(_) => Left(())
        case Left(_) => Right((i, ()))
      }
    }
  }

  /**
   * Take a parser and returns a parser,
   * which repeatedly applies the parser on the input until it is unsuccessful
   * This parser can never fail, if the parser fails the first time it'll return an empty list
   * If you're looking for a parser which fails in the case the first time parser fails use `someP`
   */
  def manyP[In, Out, Err](p: Par[In, Out, Err]): Par[In, Iterator[Out], Err] = {
    Par(i => {
      var input = i
      val successes = Iterator
        .from(0)
        .map(_ => p)
        .map(p => for {
          parseResult <- p(i)
          (in, out) = parseResult
          input = in
        } yield out)
        .takeWhile(_.isRight)
        .map(_.right.get)
      Right((input, successes))
    })
  }

  /**
   * same as `manyP`, but fails if the first try of the parser fails
   */
  def someP[In, Out, Err](p: Par[In, Out, Err]): Par[In, Iterator[Out], Unit] = {
    Par(i => {
      manyP(p)(i)
        .flatMap { case (in, outs) => {
          Either.cond(
            outs.length == 0,
            (i, outs),
            ()
          )
        }
        }
        .mapLeft(_ => ())
    })
  }

  /**
   * Takes a list of parsers and returns the first successful one.
   * In the case then no suitable parsers succeeds, the list of all failed parses is returned
   */
  def altP[In, Out, Err](ps: Par[In, Out, Err]*): Par[In, Out, Seq[Err]] = {
    Par { i => {
      val errs = ps
        .takeWhile(_.p(i).isLeft)
        .map(_.p(i))
        .map(_.left.get)

      Try(
        ps(errs.length - 1)
      )
        .toEither
        .flatMap(_(i))
        .mapLeft(_ => errs)
    }
    }
  }

  def charP(ch: Char): Par[String, Char, Unit] = {
    Par {
      case "" => Left(())
      case s => Right(s.take(1), ch)
    }
  }

  def intP: Par[String, Int, Unit] = {
    Par { i =>
      Try(i.toInt)
        .toEither
        .map((i.takeWhile(_.isDigit), _))
        .mapLeft(_ => ())
    }
  }

  def stringP(str: String): Par[String, String, Unit] = {
    Par { i =>
      Either.cond(
        i.startsWith(str),
        (str.take(str.length), str),
        ()
      )
    }
  }

  def failP[In, Out, Err](e: Err): Par[In, Out, Err] = {
    Par(_ => Left(e))
  }

}

package Parser {

  import Helpers._

  case object ParObj {
    def get[In, Err]: Par[In, In, Err] = {
      Par((i) => Right((i, i)))
    }
  }

  case class Par[In, Out, Err](p: In => Either[Err, (In, Out)]) {
    def apply(in: In): Either[Err, (In, Out)] = this.p(in)

    def map[B](f: Out => B): Par[In, B, Err] = {
      Par((input) => this.p(input).map(_.second(f)))
    }

    def mapErr[L](f: Err => L): Par[In, Out, L] = {
      Par(this.p(_).mapLeft(f))
    }

    //    def mapIn[I](f: In => I): Par[I, Out, Err] = {
    //      val p: In => Either[Err, (I, Out)] = (i) => this(f(i))
    //      ???
    //    }

    def flatMap[B](f: Out => Par[In, B, Err]): Par[In, B, Err] = {
      Par((input: In) =>
        for {
          parseResult1 <- this.p(input)
          (in1, out1) = parseResult1
          ret <- f(out1).p(in1)
        } yield ret
      )
    }

    def filter(f: Out => Boolean): Par[In, Out, Unit] = {
      Par((input) => {
        this.p(input).flatMap { t =>
          Either.cond(f(t._2), t, ())
        }.mapLeft(_ => ())
      })
    }

    def withFilter(f: Out => Boolean): Par[In, Out, Unit] = {
      this.filter(f)
    }
  }
}

package object Helpers {
  implicit class EitherExt[A, B](either: Either[A, B]) {
    def mapLeft[C](f: A => C): Either[C, B] = {
      either match {
        case Left(l) => Left(f(l))
        case Right(r) => Right(r)
      }
    }
  }

  implicit class Tuple2ExtSecond[A, B](val t: (A, B)) {
    def second[C](f: B => C): (A, C) = {
      (t._1, f(t._2))
    }
  }
}
