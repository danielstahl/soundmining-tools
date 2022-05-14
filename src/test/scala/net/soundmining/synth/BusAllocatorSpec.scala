package net.soundmining.synth

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BusAllocatorSpec extends AnyFlatSpec with Matchers {

  "BusAllocator" should "allocate a mono track" in {
    val allocator = BusAllocator(16)
    allocator.allocate(1, 0, 2) should be(Seq(16))
  }

  it should "allocate a stereo track" in {
    val allocator = BusAllocator(16)
    allocator.allocate(2, 0, 2) should be(Seq(16, 17))
  }

  it should "detect simple channels reuse" in {
    val allocator = BusAllocator(16)
    allocator.allocate(1, 0, 1.99f)
    allocator.allocate(1, 2, 4) should be(Seq(16))
  }

  it should "detect simple channel collision" in {
    val allocator = BusAllocator(16)
    allocator.allocate(1, 0, 2)
    allocator.allocate(1, 1.5F, 3.5F) should be(Seq(17))
  }

  it should "detect very simple channel collision" in {
    val allocator = BusAllocator(0)
    allocator.allocate(1, 0.0f, 1.0f)
    allocator.allocate(1, 0.0f, 1.0f) should be(Seq(1))
  }

  it should "detect end channel collision" in {
    val allocator = BusAllocator(0)
    allocator.allocate(1, 5.8865767f, 6.540641f)
    allocator.allocate(1, 5.886576f, 6.671453f) should be(Seq(1))
  }

  it should "avoid exact match" in {
    val allocator = BusAllocator(0)
    allocator.allocate(1, 11.119088f, 15.370504f)
    allocator.allocate(1, 15.370504f, 0.1f) should be(Seq(1))
  }
}
