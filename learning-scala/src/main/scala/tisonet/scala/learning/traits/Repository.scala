package tisonet.scala.learning.traits

import scala.collection.mutable

class DB {

  var people = Array(Person("Zdenek", surname = "Tison"), Person("Martina", "Tison"))

  def findOne(surname: String) = people.find(p => p.surname == surname).get
  def add(person: Person) = people = people :+ person
  def remove(person: Person) = people = people.filter(p => p != person)
}

class Repository (val repo: DB) extends ReadOnly

trait ReadOnly {
  val repo: DB

  def findOne (surname: String) = repo findOne surname
}

trait Updatable extends ReadOnly {
  def add (person: Person) = repo add person

  def -= (person: Person) = repo remove person
}

trait CachedRepository extends Updatable {
  var cache = mutable.Map[String, Person]()

  override def findOne (surname: String) = cache.getOrElseUpdate(surname, super.findOne(surname) )

  override def add (person: Person) = {
    removeKeyFromCache(person)
    super.add(person)
  }

  override def -=(person: Person) = {
    removeKeyFromCache(person)
    super.-=(person)
  }

  private def removeKeyFromCache(person: Person): Unit = {
    cache = cache - person.surname
  }
}

case class Person(firstName: String, surname: String)

object Repository {
  def readOnlyRepository = new Repository(new DB()) with ReadOnly
  def updatableRepository = new Repository(new DB()) with Updatable

}