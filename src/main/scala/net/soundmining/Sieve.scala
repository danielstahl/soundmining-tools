package net.soundmining

trait Sieve {
  def makeSieve(max: Int): Seq[Int]
  def isSieve(index: Integer): Boolean
}

case class SimpleSieve(step: Int, offset: Int) extends Sieve {
  override def makeSieve(max: Int): Seq[Int] =
    offset to max by step

  override def isSieve(index: Integer): Boolean =
    (index + offset) % step == 0
}

case class UnionSieve(sieves: Seq[Sieve]) extends Sieve {
  override def makeSieve(max: Int): Seq[Int] =
    sieves.map(_.makeSieve(max)).reduceLeft((left, right) => (left ++ right).distinct).sorted

  override def isSieve(index: Integer): scala.Boolean =
    sieves.exists(_.isSieve(index))
}

case class IntersectionSieve(sieves: Seq[Sieve]) extends Sieve {
  override def makeSieve(max: Int): Seq[Int] =
    sieves.map(_.makeSieve(max)).reduceLeft((left, right) => left.intersect(right)).sorted


  override def isSieve(index: Integer): Boolean =
    sieves.forall(_.isSieve(index))
}