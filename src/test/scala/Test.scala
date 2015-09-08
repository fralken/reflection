import org.scalatest.{Matchers, FlatSpec}

// this cannot be an inner class
case class Person(name: String, age: Int = 18)

// when using the macro version, obviously the exception is raised at compile time
class PersonNoCase(name: String, age: Int = 18)

class Test extends FlatSpec with Matchers {

  "CaseClass w/ macros" should "create an instance" in {
    val name = "Alice"
    val age = 20
    CaseClass.instance[Person](Map("name" -> name, "age" -> age)) should be (Person(name, age))
  }

  it should "create an instance with default parameters" in {
    val name = "Bob"
    CaseClass.instance[Person](Map("name" -> name)) should be (Person(name))
  }

  it should "raise NoSuchElementException when missing parameters" in {
    intercept[NoSuchElementException] {
      CaseClass.instance[Person](Map())
    }
  }

  "CaseClass w/o macros" should "create an instance" in {
    val name = "Carol"
    val age = 30
    CaseClass.instanceNoMacro[Person](Map("name" -> name, "age" -> age)) should be (Person(name, age))
  }

  it should "create an instance with default parameters" in {
    val name = "Dave"
    CaseClass.instanceNoMacro[Person](Map("name" -> name)) should be (Person(name))
  }

  it should "raise NoSuchElementException when missing parameters" in {
    intercept[NoSuchElementException] {
      CaseClass.instanceNoMacro[Person](Map())
    }
  }

  it should "raise IllegalArgumentException when trying to instance a non-case class" in {
    intercept[IllegalArgumentException] {
      val name = "Eve"
      CaseClass.instanceNoMacro[PersonNoCase](Map("name" -> name))
    }
  }
}
