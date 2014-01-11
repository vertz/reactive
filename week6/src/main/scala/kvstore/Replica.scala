package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout
import scala.collection.immutable.Nil
import scala.util.Random

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]
  val persistence = context.actorOf(persistenceProps)
  var seqNo = 0
  
  case class GlobalAck(src: ActorRef, initialWaiting: Set[ActorRef], successmsg: Any,
                       persistmsg: Any, failedmsg: Any = Nil) {
    
    var waiting : Set[ActorRef] = initialWaiting
    
    def acked = waiting.isEmpty
    def completed(actor : ActorRef) = waiting -= actor
    def retry = persistence ! persistmsg
    def fail = src ! failedmsg
    
    def completeAndSent = 
      if(acked) {
          src ! successmsg 
          true
      }
      else false
  }
  
  // key => acks 
  var acks = Map.empty[String,GlobalAck] 
  
  def waitingForAck(key: String, src: ActorRef, initialWaiting: Set[ActorRef], successmsg: Any, 
      persistmsg: Any, failedmsg: Any = Nil, hasfail: Boolean = false){
    
    acks += key -> GlobalAck(src,initialWaiting,successmsg,persistmsg,failedmsg)
    if(hasfail){
        context.system.scheduler.scheduleOnce(1.second){
            if(acks.contains(key)){
                acks(key).fail
                acks -= key
            }
        }
    }
  }
  
  def completedAck(key:String,src: ActorRef) = {
    if(acks.contains(key)){
        acks(key).completed(src)
        if(acks(key).completeAndSent) {
            acks -= key
        }
    }
  }
  
  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Insert(key, value, id) => {
        kv += ((key, value))
        persistence ! Persist(key,Some(value),id)
        secondaries.values foreach { _ ! Replicate(key, Some(value), id)}
        waitingForAck(key, sender, replicators+persistence, OperationAck(id), 
                      Persist(key, Some(value), id), OperationFailed(id), true)
    }
    case Remove(key, id) => {
        kv -= key
        persistence ! Persist(key,None,id)
        secondaries.values foreach{ _ ! Replicate(key, None, id)}
        waitingForAck(key, sender, replicators+persistence, OperationAck(id), 
                      Persist(key, None, id), OperationFailed(id), true)
    }
    case Get(key, id) =>  sender ! GetResult(key, kv.get(key), id)
    case Persisted(key, id) => completedAck(key, persistence)
    case Replicated(key, id) => completedAck(key, sender)
    case Replicas(replicas) => {
        val added = replicas -- secondaries.keySet -self
        val removed = secondaries.keySet-- replicas
      
        removed foreach {
            replica =>
                acks.keys foreach {completedAck(_, secondaries(replica))}
                replicators -= secondaries(replica)
                context.stop(secondaries(replica))
                secondaries -= replica
        }
        
        added foreach {
            replica => 
                val replicator = context.actorOf(Replicator.props(replica))
                secondaries += replica -> replicator
                replicators += replicator
                kv foreach{
                  case (k, v) => replicator ! Replicate(k, Some(v), Random.nextLong)
                }
        }
    }
  }

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Snapshot(key, valueoption, id) => {
        if(id > seqNo) {}
        else if(id < seqNo){
            sender ! SnapshotAck(key, id)
        }
        else{
            valueoption match {
                case Some(value) => kv += key->value
                case None => kv -= (key)
            }
            seqNo += 1
            val persistmsg = Persist(key, valueoption, id)
            val ackmsg = SnapshotAck(key, id)
            persistence ! persistmsg
            waitingForAck(key, sender, Set(persistence), SnapshotAck(key, id), persistmsg)
        }
    }
    case Get(key, id) => sender ! GetResult(key, kv.get(key), id)
    case Persisted(key, id) => completedAck(key, persistence)
  }
  
  def resend = {
    acks.values foreach {_.retry}
  }
  
  override def preStart = {
    arbiter ! Join
    context.system.scheduler.schedule(0.milliseconds, 100.milliseconds)(resend)
  }

}
