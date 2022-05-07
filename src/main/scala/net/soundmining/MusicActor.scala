package net.soundmining

import com.typesafe.scalalogging.LazyLogging
import net.soundmining.MusicActor.MusicActorPattern
import net.soundmining.Pattern.PatternType
import Pattern._

/**
 * Created by danielstahl on 15/03/15.
 */
object MusicActor {
  type MusicActorPattern = PatternType[MusicActor]

  val emptyActor: MusicActorPattern = constant(EmptyActor)

  def withActor(actor: MusicActor)(fn: MusicActor => MusicActor): MusicActor = {
    fn(actor)
    actor
  }
}

/**
 * Base trait for all actors.
 */
trait MusicActor extends LazyLogging {
  protected def receive: PartialFunction[MusicEvent, Unit]

  private def aroundReceive(event: MusicEvent): Unit = receive.applyOrElse(event, unhandled)

  private def unhandled(event: MusicEvent): Unit = {
    event match {
      case _  => sys.error(s"$event is unhandled by ${this.getClass.getSimpleName}")
    }
  }

  def listen(actor: MusicActor): MusicActor
  def listen(l: MusicActorPattern): MusicActor

  def tell(event: MusicEvent): Unit =  {
    logger.trace(s"$event -> $this")
    aroundReceive(event)
  }
}

trait NodeActor extends MusicActor {
  var listeners: MusicActorPattern

  def listen(l: MusicActorPattern): NodeActor = {
    listeners = l
    this
  }

  def listen(actor: MusicActor): MusicActor = {
    listeners = constant(actor)
    actor
  }
}

trait LeafActor extends MusicActor {
  override def listen(actor: MusicActor): MusicActor = sys.error(s"$this cant send a events to $actor because it is a LeafActor")
  override def listen(l: MusicActorPattern): MusicActor = sys.error(s"$this cant send a events to $l because it is a LeafActor")
}

object EmptyActor extends LeafActor {
  def receive: PartialFunction[MusicEvent, Unit] = {
    case event: MusicEvent =>
  }
}

trait MusicEvent {
}


object PrinterActor extends LeafActor {
  def receive: PartialFunction[MusicEvent, Unit] = {
    case event: MusicEvent => logger.info(s"$event")
  }
  override def toString = "PrinterActor"
}