package net.soundmining.synth

import de.sciss.osc._
import de.sciss.osc.Implicits._


case class SuperColliderClient(dumpServer: Boolean = false) {
    import SuperColliderClient._
    val DELAY: Long = 2000
    var client: UDP.Client = _
    var clockTime: Long = _
    val bufferedPlayback: BufferedPlayback = BufferedPlayback(this)

    def start(): Unit = {
        val cfg = UDP.Config()
        cfg.codec = PacketCodec().doublesAsFloats().booleansAsInts().arrays()
        this.client = UDP.Client("127.0.0.1" -> 57110, cfg)
        client.connect()
        client.action = reply
        if(dumpServer) send(dumpOSC(true))
        send(scNotify(true))
        resetClock()
        bufferedPlayback.start()
    }

    def resetClock(): Unit = {
        bufferedPlayback.reset()
        clockTime = System.currentTimeMillis()
        BusAllocator.audio.resetAllocations()
        BusAllocator.control.resetAllocations()
    }

    def send(packet: Packet): Unit = 
        client ! packet
        
    def stop(): Unit = {
        bufferedPlayback.stop()
        client.close()
    }

    def reply(packet: Packet): Unit =
        println(packet)

    def playBundle(deltaTime: Long, messages: Seq[Seq[Any]]): Unit =
        bufferedPlayback.playBundle(deltaTime, messages)

    def bundle(deltaTime: Long, packets: Packet*): Bundle =
        Bundle.millis(clockTime + DELAY + deltaTime, packets: _*)
        
    def newBundle(deltaTime: Long, messages: Seq[Seq[Any]]): Bundle = {
        val oscMessages = messages.map(message => newSynthRaw(message:_*))
        bundle(deltaTime, oscMessages:_*)
    }
}

object SuperColliderClient {
    def newSynth(instrumentName: String, addAction: Int, nodeId: Int, args: Any*): Message =  {
        val allArgs = Seq(instrumentName, -1, addAction, nodeId) ++ args
        Message("/s_new", allArgs:_*)
    }
        
    def newSynthRaw(args: Any*): Message = 
        Message("/s_new", args:_*)

    def groupHead(groupId: Int, nodeId: Int): Message =
        Message("/g_new", nodeId, 0, groupId)

    def groupTail(groupId: Int, nodeId: Int): Message =
        Message("/g_new", nodeId, 3, groupId)

    def addHeadNode(groupId: Int, nodeId: Int): Message =
        Message("/g_head", groupId, nodeId)
        
    def addTailNode(groupId: Int, nodeId: Int): Message =
        Message("/g_tail", groupId, nodeId)    

    def freeAll(nodeId: Int): Message =
        Message("/g_freeAll", nodeId)

    def allocBuffer(bufferNumber: Int, numberOfFrames: Int, numberOfChannels: Int = 2): Message =
        Message("/b_alloc", bufferNumber, numberOfFrames, numberOfChannels)    

    def freeBuffer(bufferNumber: Int): Message =
        Message("/b_free", bufferNumber)    

    def allocRead(bufferNumber: Int, pathName: String): Message =
        Message("/b_allocRead", bufferNumber, pathName)

    def loadDir(pathName: String): Message = 
        Message("/d_loadDir", pathName)    
    
    def dumpOSC(enable: Boolean): Message =
        Message("/dumpOSC", enable)
    
    def scNotify(enable: Boolean): Message =
        Message("/notify", enable)
}