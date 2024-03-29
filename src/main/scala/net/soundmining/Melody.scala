package net.soundmining

/**
 * Utils for working with melodies
 */
object Melody {
  def transpose(amount: Int, melody: Seq[Int]): Seq[Int] = {
    melody.map(_ + amount)
  }

  def shift(amount: Int, melody: Seq[Int]): Seq[Int] = {
    val (beginning, end) = melody.splitAt(amount)
    end ++ beginning
  }

  def inverse(melody: Seq[Int]): Seq[Int] = {
    val intervals = makeIntervals(melody)
    var result = Seq(intervals.head)
    var last = intervals.head
    intervals.tail.foreach {
      item =>
        val tmp = last + (item * -1)
        last = tmp
        result = result ++ Seq(tmp)
    }
    result
  }


  def makeIntervals(melody: Seq[Int]): Seq[Int] = {
    var lastItem = melody.head
    var result = Seq(melody.head)

    melody.tail.foreach {
      item =>
        val interval = item - lastItem
        lastItem = item
        result = result ++ Seq(interval)
    }
    result
  }


  def retrograde[T](melody: Seq[T]): Seq[T] = melody.reverse

  def concrete(indices: Seq[Int], spectrum: Seq[Double]): Seq[Double] = {
    indices.map(spectrum(_))
  }

  def absolute(start: Double, relative: Seq[Double]): Seq[Double] = {
    var tmp = start
    relative.map {
      time =>
        val result = tmp
        tmp = tmp + time
        result
    }
  }

}
