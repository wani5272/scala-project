import scala.io.StdIn

case class User(id: Int, name: String, age: Int)

object scalaApp {
  // TODO: make immutable
  private var database: List[User] = List()

  // the following function is NOT purely functional (side effects)
  def readInput(prompt: String): String = {
    println(prompt)
    StdIn.readLine()
  }

  // Input: User Name, Age. Output: adds entry to database (not yet immutable)
  def addUser(name: String, age: Int): List[User] = {
    // determines the next ID, if empty
    val nextId = if (database.isEmpty) 1 else database.map(_.id).max + 1
    // TODO: this line is dangerous: replacing the database with its new state. make immutable
    database = database :+ User(nextId, name, age)
    database
  }

  def listUsers(): List[User] = {
    if (database.isEmpty) {
      println("No users found.")
      List()
    } else {
      database.foreach(user => println(s"ID: ${user.id}, Name: ${user.name}, Age: ${user.age}"))
      database
    }
  }
  def updateUser(id: Int, name: String, age: Int): List[User] = {
    database.find(_.id == id) match {
      case Some(_) =>
        database = database.map(user => if (user.id == id) user.copy(name = name, age = age) else user)
        println(s"User with ID $id updated")
      case None =>
        println(s"No user found with ID $id.")
    }
    database
  }
  def deleteUser(id: Int): List[User] = {
    val initialSize = database.size
    database = database.filterNot(_.id == id)
    if (database.size < initialSize)
      // TODO: Repetition? output should be in seperate functions (or main)
      println(s"User with ID $id deleted.")
    else {
      println(s"No user found with ID $id.")
    }
    database
  }
  def main(args: Array[String]): Unit = {
    var running = true
    while (running) {
      println("\n=== In-Memory Database Menu ===")
      println("1. Add User")
      println("2. List Users")
      println("3. Update User")
      println("4. Delete User")
      println("5. Exit")

      readInput("Choose an option (1-5):") match {
        case "1" =>
          val name = readInput("Enter user name:")
          val age = readInput("Enter user age:").toInt
          addUser(name, age)

        case "2" =>
          listUsers()

        case "3" =>
          val id = readInput("Enter user ID to update:").toInt
          val name = readInput("Enter new user name:")
          val age = readInput("Enter new user age:").toInt
          updateUser(id, name, age)

        case "4" =>
          val id = readInput("Enter user ID to delete:").toInt
          deleteUser(id)
        case "5" =>
          running = false
          println("Exiting the application.")
        case _ =>
          println("Invalid option. Please choose a number between 1 and 5")
      }
    }
  }
}