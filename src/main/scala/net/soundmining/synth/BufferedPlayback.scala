package net.soundmining.synth

import de.sciss.osc.Bundle

import java.util.concurrent.{Executors, PriorityBlockingQueue, ScheduledExecutorService, ScheduledFuture, TimeUnit}

case class BufferedPlayback(client: SuperColliderClient) {
  val BUFFER_TIME: Long = 1000 * 30
  val bundleQueue: PriorityBlockingQueue[ComparableBundle] = new PriorityBlockingQueue[ComparableBundle]()

  var scheduler: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor()
  var scheduledFuture: Option[ScheduledFuture[_]] = None

  def playBundle(deltaTime: Long, messages: Seq[Seq[Any]]): Unit =
    bundleQueue.offer(ComparableBundle(client.newBundle(deltaTime, messages)))

  def peekTime(): Option[Long] = {
    if(bundleQueue.isEmpty) None
    else Some(bundleQueue.peek().bundle.timeTag.toMillis)
  }

  def peekOverdue(): Boolean =
    peekTime().exists(time => (time - BUFFER_TIME) < System.currentTimeMillis())

  val playDueBundles: Runnable = () => {
    while(peekOverdue()) {
      client.send(bundleQueue.poll().bundle)
    }
  }

  def start(): Unit = {
    bundleQueue.clear()
    scheduledFuture = Some(scheduler.scheduleAtFixedRate(playDueBundles, 1000, 5000, TimeUnit.MILLISECONDS))
  }

  def reset(): Unit = {
    scheduledFuture.foreach(future => future.cancel(true))
    bundleQueue.clear()
    scheduledFuture = Some(scheduler.scheduleAtFixedRate(playDueBundles, 1000, 5000, TimeUnit.MILLISECONDS))

  }

  def stop(): Unit = {
    scheduledFuture.foreach(future => future.cancel(true))
    scheduledFuture = None
    bundleQueue.clear()
    scheduler.shutdown()
  }

  case class ComparableBundle(bundle: Bundle) extends Comparable[ComparableBundle] {
    override def compareTo(o: ComparableBundle): Int = {
      bundle.timeTag.toMillis.compareTo(o.bundle.timeTag.toMillis)
    }
  }
}
