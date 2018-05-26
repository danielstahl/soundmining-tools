package net.soundmining

import java.net.InetAddress
import java.util.concurrent.PriorityBlockingQueue
import java.{util => ju}

import com.illposed.osc.{OSCBundle, OSCMessage, OSCPacket, OSCPortOut}

import scala.collection.JavaConversions._

case class MusicPlayer() {
  val sender: OSCPortOut = new OSCPortOut(InetAddress.getLoopbackAddress, 57110)

  val clock: PlayerClock = PlayerClock()

  def play(playable: Playable): Unit = {
    clock.reset()
    playable.play()(this)
  }

  def play(playable: (MusicPlayer) => Unit): Unit = {
    play(
      new Playable {
        override def play()(implicit player: MusicPlayer): Unit = playable(player)
      }
    )
  }

  def startPlay(): Unit = {
    clock.reset()
    playThread.start()
  }

  def sendNew(deltaTime: Long, messages: Seq[Seq[Object]]) =
    sendBundle(deltaTime, messages.map(message => makeNew(message)))

  def sendNewSingle(deltaTime: Long, message: Seq[Object]) =
    sendBundle(deltaTime, Seq(makeNew(message)))

  private def makeOSCMessage(name: String, arguments: Seq[Object]): OSCMessage = {
    val theMessages = new ju.ArrayList[Object](arguments.size)
    theMessages.addAll(arguments)
    new OSCMessage(name, theMessages)
  }

  private def makeOSCBundle(messages: Seq[OSCPacket], theTime: ju.Date) = {
    val theMessages = new ju.ArrayList[OSCPacket](messages.size)
    theMessages.addAll(messages)
    new OSCBundle(theMessages, theTime)
  }

  def makeNew(arguments: Seq[Object]) =
    makeOSCMessage("/s_new", arguments)

  def makeGroupHead(groupId: Integer, nodeId: Integer) =
    makeOSCMessage("/g_new", Seq(nodeId, new Integer(0), groupId))

  def makeGroupTail(groupId: Integer, nodeId: Integer) =
    makeOSCMessage("/g_new", Seq(nodeId, /*new Integer(1)*/ new Integer(3), groupId))

  def makeAddHeadNode(groupId: Integer, nodeId: Integer) =
    makeOSCMessage("/g_head", Seq(groupId, nodeId))

  def makeAddTailNode(groupId: Integer, nodeId: Integer) =
    makeOSCMessage("/g_tail", Seq(groupId, nodeId))

  def makeFreeAll(nodeId: Integer) =
    makeOSCMessage("/g_freeAll", Seq(nodeId))

  def makeAllocBuffer(bufferNumber: Integer, numberOfFrames: Integer, numberOfChannels: Integer = 2) =
    makeOSCMessage("/b_alloc", Seq(bufferNumber, numberOfFrames, numberOfChannels))

  def makeWriteSoundFile(bufferNumber: Integer, pathName: String, headerFormat: String = "aiff", sampleFormat: String = "float",
                         numberOfFrames: Integer = -1, startingFrameInBuffer: Integer = 0, leaveFileOpen: Integer = 1) =
    makeOSCMessage("/b_write", Seq(bufferNumber, pathName, headerFormat, sampleFormat, numberOfFrames, startingFrameInBuffer, leaveFileOpen))

  def makeFreeBuffer(bufferNumber: Integer) =
    makeOSCMessage("/b_free", Seq(bufferNumber))

  def makeAllocRead(bufferNumber: Integer, pathName: String) =
    makeOSCMessage("/b_allocRead", Seq(bufferNumber, pathName))

  case class ComparableBundle(bundle: OSCBundle) extends Comparable[ComparableBundle] {
    override def compareTo(o: ComparableBundle): Int = {
      bundle.getTimestamp.compareTo(o.bundle.getTimestamp)
    }
  }

  val playQueue: PriorityBlockingQueue[ComparableBundle] = new PriorityBlockingQueue()

  val playThread = new Thread(new Runnable {
    val BUFFER_TIME = 1000 * 30

    def peekTime: Option[ju.Date] = {
      Option(playQueue.peek).map {
        bundle => bundle.bundle.getTimestamp
      }
    }

    def peekOverdue: Boolean = {
      peekTime.exists {
        time => (time.getTime - BUFFER_TIME) < System.currentTimeMillis()
      }
    }

    override def run() = {
      println(s"Starting background player")
      while (true) {
        while (peekOverdue) {
          sender.send(playQueue.poll().bundle)
        }
        Thread.sleep(1000)
      }
    }
  })

  def sendBundle(deltaTime: Long, messages: Seq[OSCPacket]) = {
    val theTime = new ju.Date(clock.start + deltaTime)
    playQueue.offer(ComparableBundle(makeOSCBundle(messages, theTime)))
  }
}

case class PlayerClock() {
  val DELAY: Long = 2000
  var start: Long = System.currentTimeMillis() + DELAY

  def reset() = start = System.currentTimeMillis() + DELAY
}

trait Playable {
  def play()(implicit player: MusicPlayer)
}
