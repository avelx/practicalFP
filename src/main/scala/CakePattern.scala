package com.avel.patterns

final case class User(username: String, password: String)

trait UserRepositoryComponent {
  val userRepository: UserRepository

  class UserRepository {
    def authenticate(user: User): Option[User] = {
      println("authenticating user: " + user)
      Some(user)
    }

    def create(user: User) = println("creating user: " + user)

    def delete(user: User) = println("deleting user: " + user)
  }

}

trait UserServiceComponent {
  this: UserRepositoryComponent =>
  val userService: UserService

  class UserService {
    def authenticate(userName: String, pass: String): Boolean =
      userRepository
        .authenticate(User(userName, pass))
        .isDefined

    def create(username: String, password: String) =
      userRepository.create(new User(username, password))

    def delete(user: User) = userRepository.delete(user)
  }

}

object ComponentRegistry extends UserServiceComponent with UserRepositoryComponent {
  override val userService = new UserService
  override val userRepository = new UserRepository
}


object CakePattern {
  def main(args: Array[String]): Unit = {
  }
}
