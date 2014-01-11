package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt
  def flip(prob: Double): Boolean = prob >= math.random

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    
    val roomNeighbors: Int = 4
    
    val incubationTime: Int = 6
    val dieTime: Int = 14 - incubationTime
    val immuneTime: Int = 16 - immuneTime
    val healTime: Int = 18 - healTime
    
    val prevalenceRate: Double = 0.01
    val transRate: Double = 0.4
    val dieRate: Double = 0.25
    
    val daysPerMove: Int = 5
  }

  import SimConfig._

  
  
  val persons: List[Person] = initPersons()
  
  def initPersons(): List[Person] = {

    val persons = (0 to population).map(new Person(_)).toList
    var count = (prevalenceRate * population).toInt
    
    while(count > 0){
      val rand = randomBelow(population)
      if(!persons(rand).infected){
            persons(rand).infected = true
            count = count - 1
      }
    }
    
    persons
  }

  case class Room(row: Int, col: Int) {
    private def neighbors(x1: Int, x2: Int, max: Int): Boolean = {
      val distance = (x1 - x2).abs
      distance == 1 || distance == max - 1
    }

    override def equals(o: Any) = o match {
        case that: Room => that.row == row && that.col == col
        case _ => false
    }
    
    private def cNeighbors(room: Room) = col == room.col && neighbors(row, room.row, roomRows) 
    private def rNeighbors(room: Room) = row == room.row && neighbors(col, room.col, roomColumns)

    def up = Room((row - 1 + roomRows) % roomRows, col)
    def down = Room((row + 1) % roomRows, col)
    def left = Room(row, (col - 1 + roomColumns) % roomColumns)
    def right = Room(row, (col + 1) % roomColumns)
    
    def isNeighbor(room: Room): Boolean = rNeighbors(room) || cNeighbors(room)
    def neighborsSet: List[Room] = List(up, down, left, right)

    def hasInfectiousPeople = persons.exists(x => equals(x room) && x.isInfectous)
    def hasVisibleInfectiousPeople = persons.exists(x => equals(x room) && x.isVisiblyInfected)
  }
  
  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def isInfectous: Boolean = infected || sick || immune || dead
    def isHealthy: Boolean = !isInfectous
    def isVisiblyInfected: Boolean = sick || dead
    
    def room: Room = Room(row, col)
    
    def init() {
      if (!dead){
        if(infected) infectedInit()
        else infectedEvent()
          
        tryMoveToNeighboringRoom()
        afterDelay(daysPerMove)(loop())
      }
    }
    
    def infectedInit(){
      if (!dead) {
        infected = true
        afterDelay(incubationTime)(sickEvent())
      }
    }
    
    def moveToRoom(room: Room) {
      row = room.row
      col = room.col
      if (isHealthy && room.hasInfectiousPeople && flip(transRate)) {
        infected = true
        afterDelay(incubationTime)(sickEvent())
      }
    }
    
    def validRooms = room.neighborsSet.filterNot(_ hasVisibleInfectiousPeople)

    def tryMoveToNeighboringRoom() {
      if (!dead){
        val rooms = validRooms
        if (!rooms.isEmpty) moveToRoom(rooms(randomBelow(rooms size)))
        else infectedEvent()
      }
    }
    
    def infectedEvent(){
      if (isHealthy && room.hasInfectiousPeople && flip(transRate)) {
        infected = true
        afterDelay(incubationTime)(sickEvent())
      }
    }

    def sickEvent() {
      if(!dead){
        sick = true
        afterDelay(dieTime)(deathEvent())
      }
    }
    
    def deathEvent() {
      if(!dead){
        if(flip(dieRate)) dead = true
        else afterDelay(immuneTime)(immuneEvent())
      }
    }
    
    def immuneEvent() {
      if(!dead){
        immune = true
        afterDelay(healTime)(healthyEvent())
      }
    }
    
    def healthyEvent() {
      if(!dead){
        infected = false
        sick = false
        immune = false
      }
    }
    
    def loop(){
      val rand = randomBelow(daysPerMove)
      afterDelay(rand)(tryMoveToNeighboringRoom())
      afterDelay(rand)(loop())
    }
    
    afterDelay(0)(init())
  }
}
