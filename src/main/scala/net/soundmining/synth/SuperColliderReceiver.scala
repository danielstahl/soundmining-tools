package net.soundmining.synth

import de.sciss.osc.{Message, Packet, PacketCodec, UDP}
import de.sciss.osc.UDP.Receiver

import java.net.SocketAddress

case class SuperColliderReceiver(reply: SuperColliderReply) {
  var oscServer: Receiver.Undirected = _

  def start(): Unit = {
    val cfg = UDP.Config()
    cfg.codec = PacketCodec().doublesAsFloats().booleansAsInts()
    cfg.localPort = 57111
    this.oscServer = UDP.Receiver(cfg)
    this.oscServer.connect()
    this.oscServer.action = oscReply
  }

  def stop(): Unit = {
    this.oscServer.close()
  }

  def oscReply(packet: Packet, socketAddress: SocketAddress): Unit = {
    packet match {
      case Message("/noteOn", key: Int, velocity: Int, device: String) =>
        reply.noteOn(key, velocity, device)
      case Message("/noteOff", key: Int, velocity: Int, device: String) =>
        reply.noteOff(key, velocity, device)
      case Message("/cc", value: Int, control: Int, device: String) =>
        reply.cc(value, control, device)
      case Message("/bend", value: Int, device: String) =>
        reply.bend(value, device)
      case _ =>
    }
  }
}

trait SuperColliderReply {
  def noteOn(key: Int, velocity: Int, device: String): Unit = {}
  def noteOff(key: Int, velocity: Int, device: String): Unit = {}
  def cc(value: Int, control: Int, device: String): Unit = {}
  def bend(value: Int, device: String): Unit = {}
}

case class PatchPlayback(patch: Patch = EmptyPatch, client: SuperColliderClient) extends SuperColliderReply {
  val STATIC_MIDI_DELAY_TIME = 1900

  def currentStartTime(): Double = {
    if (client.clockTime <= 0) client.resetClock()
    (System.currentTimeMillis() - (client.clockTime + STATIC_MIDI_DELAY_TIME)) / 1000.0
  }

  override def noteOn(key: Int, velocity: Int, device: String): Unit = {
    patch.noteHandle(currentStartTime(), key, velocity, device)
  }

  override def cc(value: Int, control: Int, device: String): Unit = {
    patch.ccHandle(value, control, device)
  }
}

trait Patch {
  def noteHandle(start: Double, key: Int, velocity: Int, device: String): Unit
  def ccHandle(value: Int, control: Int, device: String): Unit = {}
}

object EmptyPatch extends Patch{
  override def noteHandle(start: Double, key: Int, velocity: Int, device: String): Unit = {
    println(s"Note handle start $start key $key velocity $velocity device $device")
  }

  override def ccHandle(value: Int, control: Int, device: String): Unit = {
    println(s"cc handle control $control value $value device $device")
  }
}

object MidiUtils {
  val A_PITCH = 440.0

  def midiToFreq(midiNote: Int): Double =
    (A_PITCH / 32) * math.pow(2, (midiNote - 9) / 12.0)

  def midiToOctave(midiNote: Int): Int =
    (midiNote / 12) - 1
}