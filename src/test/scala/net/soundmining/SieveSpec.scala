package net.soundmining

import org.scalatest.flatspec.AnyFlatSpec

class SieveSpec extends AnyFlatSpec {
  behavior of "Simple sieve"

  it should "make a sorted sequence" in {
    val sieve = SimpleSieve(2, 0)
    assert(sieve.makeSieve(4) === Seq(0, 2, 4))
  }

  it should "make a sequence that start on offset" in {
    val sieve = SimpleSieve(2, 1)
    assert(sieve.makeSieve(4) === Seq(1, 3))
  }

  it should "choose if sieve index is true" in {
    val sieve = SimpleSieve(2, 0)
    assert(sieve.isSieve(2) === true)
    assert(sieve.isSieve(5) === false)
  }

  it should "choose if sieve index is true with offset" in {
    val sieve = SimpleSieve(2, 1)
    assert(sieve.isSieve(3) === true)
    assert(sieve.isSieve(4) === false)
  }

  behavior of "Union sieve"

  it should "make a sorted union of two sequences" in {
    val sieve = UnionSieve(Seq(SimpleSieve(2, 0), SimpleSieve(3, 0)))
    assert(sieve.makeSieve(6) === Seq(0, 2, 3, 4, 6))
  }

  behavior of "Intersection sieve"

  it should "make a sorted intersection of two sequences" in {
    val sieve = IntersectionSieve(Seq(SimpleSieve(2, 0), SimpleSieve(3, 0)))
    assert(sieve.makeSieve(6) === Seq(0, 6))
  }
}
