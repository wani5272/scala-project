import scala.io.StdIn

case class User(id: Int, name: String, age: Int, deleted: Boolean)

object scalaApp {
  private var database: List[User] = List()

  // the following function is NOT purely functional (side effects)
  def readInput(prompt: String): String = {
    var input: String = ""
    while (input == "") {
      println(prompt)
      input = StdIn.readLine()
    }
    input

  }
  def readInputAsInt(prompt: String): Integer = {

    var input: Int = -1
    while (input <= -1) {
      println(prompt)
      try {
        input = StdIn.readLine().toInt
      } catch {
        case _ => input = -1
      }
    }
    input
  }

  // Input: User Name, Age. Output: adds entry to database (not yet immutable)
  def addUser(name: String, age: Int): List[User] = {
    // determines the next ID, if empty
    val nextId = if (database.isEmpty) 1 else database.map(_.id).max + 1
    database = database :+ User(nextId, name, age, false)
    database
  }

  def noUsersFound(): Unit  = {
    println("No users found.")
  }
  def userWithIDAction(id: Int, action: String): Unit = {
    println(s"User with ID $id $action")
  }
  def noUsersFoundWithID(id: Int): Unit = {
    println(s"No users found with ID $id.")
  }
  def listUsers(): List[User] = {
    if (!database.exists(_.deleted == false)) {
      noUsersFound()
      List()
    } else {
      database.filter(_.deleted == false).foreach(user => println(s"ID: ${user.id}, Name: ${user.name}, Age: ${user.age}"))
      database
    }
  }

  // YOU ARE NOT SUPPOSED TO USE THE FOLLOWING FUNCTION
  def listAllUsers(): List[User] = {
    if (database.isEmpty) {
      noUsersFound()
      List()
    } else {
      database.foreach(user => println(s"ID: ${user.id}, Name: ${user.name}, Age: ${user.age}, deleted: ${user.deleted}"))
      database
    }
  }
  def updateUser(id: Int, name: String, age: Int): List[User] = {
    database.filter(_.deleted == false).find(_.id == id) match {
      case Some(_) =>
        database = database.map(user => if (user.id == id & !user.deleted) user.copy(name = name, age = age, deleted = false) else user)
        userWithIDAction(id, "updated")
      case None =>
        noUsersFoundWithID(id)
    }
    database
  }
  def deleteUser(id: Int): List[User] = {
    database.filter(_.deleted == false).find(_.id == id) match {
      case Some(_) =>
        database = database.map(user => if (user.id == id & !user.deleted) user.copy(deleted = true) else user)
        userWithIDAction(id, "\"deleted\"")
      case None =>
        noUsersFoundWithID(id)
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
          val age = readInputAsInt("Enter user age:")
          addUser(name, age)

        case "2" =>
          listUsers()

        case "3" =>
          val id = readInputAsInt("Enter user ID to update:")
          val name = readInput("Enter new user name:")
          val age = readInputAsInt("Enter new user age:")
          updateUser(id, name, age)

        case "4" =>
          val id = readInputAsInt("Enter user ID to delete:")
          deleteUser(id)
        case "5" =>
          running = false
          println("Exiting the application.")

        case "listAllUsers1234" =>
          println("WARNING: THIS FUNCTION IS ONLY FOR LOGGING PURPOSES!")
          listAllUsers()

        case _ =>
          println("Invalid option. Please choose a number between 1 and 5")
      }
    }
  }
}