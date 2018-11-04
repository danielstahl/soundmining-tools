package net.soundmining

import net.soundmining.Instrument._
import net.soundmining.Utils._

import java.{lang => jl}

/**
 * Basic building blocks to work with instruments
 */
object Instrument {

  /**
   * Envelope curves
   */
  sealed case class EnvCurve(name: String)
  object LINEAR extends EnvCurve("line")
  object SINE extends EnvCurve("sin")
  object EXPONENTIAL extends EnvCurve("exp")
  object WELCH extends EnvCurve("wel")
  object SQUARED extends EnvCurve("sqr")
  object CUBED extends EnvCurve("cub")

  /**
   * How the instrument is added relative to
   * other playing instruments when played
   */
  sealed case class AddAction(action: Integer)
  object HEAD_ACTION extends AddAction(new Integer(0))
  object TAIL_ACTION extends AddAction(new Integer(1))
  object BEFORE_ACTION extends AddAction(new Integer(2))
  object AFTER_ACTION extends AddAction(new Integer(3))

  /**
   * Predefined nodes to add instruments to.
   * This is important if you want instruments
   * to be able to act upon other instruments
   */
  sealed case class Node(nodeId: Integer)
  object SOURCE extends Node(1004)
  object EFFECT extends Node(1005)
  object ROOM_EFFECT extends Node(1006)

  def setupNodes(player: MusicPlayer) = {
    val osc = Seq(
      player.makeGroupHead(0, SOURCE.nodeId),
      player.makeGroupTail(SOURCE.nodeId, EFFECT.nodeId),
      player.makeGroupTail(EFFECT.nodeId, ROOM_EFFECT.nodeId))
    player.sendBundle(absoluteTimeToMillis(0f), osc)
  }

  def buildFloat(value: Float): jl.Float = new jl.Float(value)
  def buildInteger(value: Int): jl.Integer = new jl.Integer(value)
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

  var control = buildInteger(0)
  var audio = buildInteger(16)

  def reset() = {
    control = buildInteger(0)
    audio = buildInteger(16)
  }

  def nextControl(): jl.Integer = {
    val result = control
    control = buildInteger(control + 1)
    result
  }

  def nextAudio(): jl.Integer = {
    val result = audio
    audio = buildInteger(audio + 1)
    result
  }

  def currentControl: jl.Integer = control
  def currentAudio: jl.Integer = audio
}

/**
 * The basic building blocks to represent instruments
 * Has the possibility to hold other instruments that
 * will be built before them.
 */
trait InstrumentBuilder extends ArgumentBuilder {
  var instruments: Seq[InstrumentBuilder] = Seq(this)

  def addChild(instrument: InstrumentBuilder) = instruments = instruments :+ instrument
  def buildInstruments(): Seq[Seq[Object]] = instruments.reverseMap(_.build())

  def build(): Seq[Object]
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

  def build(): Seq[Object] = {
    Seq(
      instrumentName,
      new Integer(-1), addAction.action, nodeId.nodeId
    )
  }
}

/**
 * Build a duration argument. The name in
 * the SynthDef should be named "dur".
 */
trait DurBuilder extends ArgumentBuilder {
  var dur: jl.Float = buildFloat(1f)

  def dur(value: Float): SelfType = {
    dur = buildFloat(value)
    self()
  }

  def buildDur(): Seq[Object] = Seq("dur", dur)
}

/**
 * Build a output bus argument. The name
 * in the SynthDef should be named "out".
 */
trait OutputBuilder extends ArgumentBuilder {
  var out: jl.Integer = buildInteger(0)

  def out(value: Int): SelfType = {
    out = buildInteger(value)
    self()
  }

  def buildOut(): Seq[Object] = Seq("out", out)
}

/**
 * Build a input bus argument. The name
 * in the SynthDef should be named "in".
 */
trait InputBuilder extends ArgumentBuilder {
  var in: jl.Integer = buildInteger(0)

  def in(value: Int): SelfType = {
    in = buildInteger(value)
    self()
  }

  def buildIn(): Seq[Object] = Seq("in", in)
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

  var bus: jl.Integer = buildInteger(0)

  def bus(value: Int): SelfType = {
    bus = buildInteger(value)
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

  def buildBus(): Seq[Object] = Seq(
    name, bus
  )
}

/**
  * Trait for Instruments that supplies a Bus for other instruments to
  * have as input buses.
  */
trait BusSupplier {
  def bus(): Integer
}

/**
 * Basic trait for control-instruments. Will only generate output.
 */
trait ControlInstrumentBuilder extends OutputBuilder with InstrumentBuilder with DurBuilder with BusSupplier {
  def bus(): Integer = out
}

/**
 * Basic trait for a controlReplaceInstrument. Will read data from input, transform it and
 * then output the result.
 */
trait ControlReplaceInstrumentBuilder extends InputBuilder with InstrumentBuilder with DurBuilder with BusSupplier {
  def bus(): Integer = in
}