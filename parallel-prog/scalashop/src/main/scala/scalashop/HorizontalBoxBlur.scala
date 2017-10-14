package scalashop

import java.util.concurrent.ForkJoinTask

import org.scalameter._
import common._

import scalashop.VerticalBoxBlur.blur

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    var col = from
    while(col < end) {
      var row = 0
      while(row < src.width) {
        dst(row, col) = boxBlurKernel(src, row, col, radius)
        row = row + 1
      }
      col = col + 1
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    var ts = (src.height  / numTasks).max(1)
    var counter = 0
    var tasks = scala.collection.mutable.ArrayBuffer.empty[ForkJoinTask[Unit]]
    while(counter < src.height) {
      val currCounter = counter
      tasks += task {
        blur(src, dst, currCounter, (currCounter + ts).min(src.height), radius)
      }

      counter = counter + ts
    }
    tasks.foreach(t => t.join())
  }

}
