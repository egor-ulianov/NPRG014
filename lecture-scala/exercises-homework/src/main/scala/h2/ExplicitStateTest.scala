/*
Implement the classes below such that the main (without modifications) prints out the something like this:

Person John Doe aged 24
Person John Doe aged 25
List(h2.PersonState@3d24753a)
Person John Doe aged 24
Thing Box with color (255,0,0)
Person Joe aged 24

*/


package h2

import scala.collection.mutable.ListBuffer


trait WithExplicitState:
  protected type StateType
  protected def state: StateType
  protected def state_=(state: StateType): Unit


class PersonState(val name: String, val age: Int)

class Person extends WithExplicitState:
  protected type StateType = PersonState

  private var _state: PersonState = _

  def setName(name: String): this.type =
    _state = PersonState(name, if (_state != null) _state.age else 0)
    this

  def setAge(age: Int): this.type =
    _state = PersonState(if (_state != null) _state.name else "", age)
    this

  protected def state: PersonState = _state
  protected def state_=(state: PersonState): Unit =
    _state = state

  override def toString: String = s"Person ${_state.name} aged ${_state.age}"


type RGBColor = (Int, Int, Int)
class ThingState(val name: String, val color: RGBColor)

class Thing extends WithExplicitState:
  protected type StateType = ThingState

  private var _state: ThingState = _

  def setName(name: String): this.type =
    _state = ThingState(name, if (_state != null) then state.color else new RGBColor(0 ,0 ,0))
    this

  def setColor(color: RGBColor): this.type =
    _state= ThingState(if (_state != null) then _state.name else "", color)
    this

  protected def state: ThingState = _state

  protected def state_= (state: StateType): Unit =
    _state = state.asInstanceOf[ThingState]

  override def toString: String = s"Thing ${_state.name} with color ${_state.color}"


trait History extends WithExplicitState:
    /* Add necessary declarations here. This trait should have no knowledge of classes Person, Thing, PersonState, ThingState.
       It should depend only on the trait WithExplicitState.
    */

    val hist: ListBuffer[this.StateType] = ListBuffer.empty[this.StateType]

    def checkpoint(): this.type =
      hist.append(state)
      this

    def history: List[this.StateType] = hist.toList

    def restoreTo(s: this.StateType): this.type =
      state = s
      this


object ExplicitStateTest:
  def main(args: Array[String]): Unit =
    // The inferred type of variable "john" should be "Person & History".
    val john = (new Person with History).setName("John Doe").setAge(24).checkpoint()

    println(john)
    john.setAge(25)

    println(john)
    println(john.history)

    val johnsPrevState = john.history(0)
    john.restoreTo(johnsPrevState)
    println(john)

    // The inferred type of variable "box" should be "Thing & History".
    val box = new Thing with History
    box.setName("Box")
    box.setColor((255, 0, 0))
    println(box)

    val joe = new Person with History
    joe.restoreTo(johnsPrevState).setName("Joe")
    println(joe)

    // The line below must not compile. It should complain about an incompatible type.
    //box.restoreTo(johnsPrevState)
//    / Users / egorulanov / Studies / NPRG014 / lecture -scala / exercises - homework / src / main / scala / h2 / ExplicitStateTest.scala: 115
//    : 19
//    Found: (johnsPrevState: john.StateType)
//    Required: box.StateType
//    ²
//
//    where: StateType is a
//    type in
//    class Person
//    which is an alias of h2
//  .PersonState
//    StateType
//    ²is a
//    type in
//    class Thing
//    which is an alias of h2
//  .ThingState
//
//    box.restoreTo(johnsPrevState)
