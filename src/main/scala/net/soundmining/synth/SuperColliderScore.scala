package net.soundmining.synth

import de.sciss.osc.Message

import java.io.FileWriter

case class SuperColliderScore() {

  var messages: Seq[(Double, Message)] = Seq()

  def reset(): Unit = messages = Seq()

  def addMessage(deltaTime: Double, message: Message): Unit =
    messages = messages :+ (deltaTime, message)

  def addMessage(message: Message): Unit =
    messages = messages :+ (0, message)

  def makeScore(fileName: String = "tempScore.txt"): Unit = {
    val fileWriter = new FileWriter(fileName)
    fileWriter.write("[\n")

    val maxMessage = messages.maxBy {
      case (deltaTime, message) => deltaTime
    }

    val maxMessageDuration = maxMessage._2.args.zipWithIndex.find {
      case (value, index) => value == "dur"
    }.map {
      case (value, index) => maxMessage._2.args(index + 1).asInstanceOf[Double]
    }.get

    val duration = maxMessage._1 + maxMessageDuration + 10

    messages
      .sortBy(_._1)
      .map {
        case (deltaTime, message) => messageToString(deltaTime, message)
      }
      .foreach {
        str =>
          fileWriter.write(s"$str,\n")
          println(s"$str,")
      }

    fileWriter.write(s"[$duration, [\\c_set, 0, 0]]")
    fileWriter.write("]\n")
    fileWriter.close()
    println("done")
  }

  def messageToString(deltaTime: Double, message: Message): String = {
    val args = message.args
      .map {
        case arg@(_: String) =>
          if (message.name == "/d_loadDir") {
            s"'$arg'"
          } else {
            s"\\$arg"
          }
        case arg@(_: List[Any]) =>
          s"[${arg.mkString(", ")}]"

        case arg =>
          arg
      }
    s"[$deltaTime, [${message.name.replace('/', '\\')}, ${args.mkString(", ")}]]"
  }
}
