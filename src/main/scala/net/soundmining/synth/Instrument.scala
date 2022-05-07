package net.soundmining.synth

import SuperColliderClient._
import Instrument._

/**
 * Basic building blocks to work with instruments
 */
object Instrument {

  /**
   * Envelope curves
   */
  sealed case class EnvCurve(name: String)
  object LINEAR extends EnvCurve("lin")
  object SINE extends EnvCurve("sin")
  object EXPONENTIAL extends EnvCurve("exp")
  object WELCH extends EnvCurve("wel")
  object SQUARED extends EnvCurve("sqr")
  object CUBED extends EnvCurve("cub")

  /**
   * How the instrument is added relative to
   * other playing instruments when played
   */
  sealed case class AddAction(action: Int)
  object HEAD_ACTION extends AddAction(0)
  object TAIL_ACTION extends AddAction(1)
  object BEFORE_ACTION extends AddAction(2)
  object AFTER_ACTION extends AddAction(3)

  /**
   * Predefined nodes to add instruments to.
   * This is important if you want instruments
   * to be able to act upon other instruments
   */
  sealed case class Node(nodeId: Int)
  object SOURCE extends Node(1004)
  object EFFECT extends Node(1005)
  object ROOM_EFFECT extends Node(1006)

  def setupNodes(player: SuperColliderClient): Unit = {
      player.send(groupHead(0, SOURCE.nodeId))
      player.send(groupTail(SOURCE.nodeId, EFFECT.nodeId))
      player.send(groupTail(EFFECT.nodeId, ROOM_EFFECT.nodeId))
  }
}

/**
 * This is the basic type that is able to reference and
 * return one self. This is needed if we want to build
 * a good DSL.
 */
trait ArgumentBuilder {
  type SelfType <: InstrumentBuilder
  def self(): SelfType
}

/**
 * Generate buses
 */
object BusGenerator {

  var control: Int = 0
  var audio: Int = 16

  def reset(): Unit = {
    control = 0
    audio = 16
  }

  def nextControl(): Int = {
    val result = control
    control = control + 1
    result
  }

  def nextAudio(): Int = {
    val result = audio
    audio = audio + 1
    result
  }

  def currentControl: Int = control
  def currentAudio: Int = audio
}

/**
 * The basic building blocks to represent instruments
 * Has the possibility to hold other instruments that
 * will be built before them.
 */
trait InstrumentBuilder extends ArgumentBuilder {
  var instruments: Seq[InstrumentBuilder] = Seq(this)

  def addChild(instrument: InstrumentBuilder): Unit = instruments = instruments :+ instrument
  def buildInstruments(): Seq[Seq[Any]] = instruments.reverseMap(_.build())

  def build(): Seq[Any]
}

/**
 * Base instrument. The instrumentName correspond to
 * The name of the SynthDef in SuperCollider
 */
abstract class AbstractInstrumentBuilder extends InstrumentBuilder {
  val instrumentName: String

  var addAction: AddAction = HEAD_ACTION

  def addAction(value: AddAction): SelfType = {
    addAction = value
    self()
  }

  var nodeId: Node = SOURCE

  def nodeId(value: Node): SelfType = {
    nodeId = value
    self()
  }

  def build(): Seq[Any] = {
    Seq(
      instrumentName,
      -1, addAction.action, nodeId.nodeId
    )
  }
}

/**
 * Build a duration argument. The name in
 * the SynthDef should be named "dur".
 */
trait DurBuilder extends ArgumentBuilder {
  var dur: Double = 1

  def dur(value: Double): SelfType = {
    dur = value
    self()
  }

  def buildDur(): Seq[Any] = Seq("dur", dur)
}

/**
 * Build a output bus argument. The name
 * in the SynthDef should be named "out".
 */
trait OutputBuilder extends ArgumentBuilder {
  var out: Int = 0

  def out(value: Int): SelfType = {
    out = value
    self()
  }

  def buildOut(): Seq[Any] = Seq("out", out)
}

/**
 * Build a input bus argument. The name
 * in the SynthDef should be named "in".
 */
trait InputBuilder extends ArgumentBuilder {
  var in: Int = 0

  def in(value: Int): SelfType = {
    in = value
    self()
  }

  def buildIn(): Seq[Any] = Seq("in", in)
}

/**
 * Allows you to have a argument in your instrument
 * that is a "control-instrument". In your instrument
 * it will a control-bus but the ControlInstrumentBuilders that
 * you add will be built and output first and will
 * have the same control-bus.
 */
case class ControlArgumentBuilder[ST <: InstrumentBuilder](me: ST, name: String) extends ArgumentBuilder {
  override type SelfType = ST
  override def self(): SelfType = me

  var bus: Int = 0

  def bus(value: Int): SelfType = {
    bus = value
    self()
  }

  def control(controlInstrument: ControlInstrumentBuilder, controlReplaceInstruments: ControlReplaceInstrumentBuilder*): SelfType = {
    val outBus = BusGenerator.nextControl()
    controlInstrument.out(outBus)
    bus(outBus)
    me.addChild(controlInstrument)
    controlReplaceInstruments.foreach {
      controlReplaceInstrument =>
        controlReplaceInstrument.in(outBus)
        me.addChild(controlReplaceInstrument)
    }
    self()
  }

  def buildBus(): Seq[Any] = Seq(
    name, bus
  )
}

/**
  * Trait for Instruments that supplies a Bus for other instruments to
  * have as input buses.
  */
trait BusSupplier {
  def bus(): Int
}

/**
 * Basic trait for control-instruments. Will only generate output.
 */
trait ControlInstrumentBuilder extends OutputBuilder with InstrumentBuilder with DurBuilder with BusSupplier {
  def bus(): Int = out
}

/**
 * Basic trait for a controlReplaceInstrument. Will read data from input, transform it and
 * then output the result.
 */
trait ControlReplaceInstrumentBuilder extends InputBuilder with InstrumentBuilder with DurBuilder with BusSupplier {
  def bus(): Int = in
}