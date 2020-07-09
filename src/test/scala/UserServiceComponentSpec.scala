import com.avel.patterns._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalamock.scalatest.MockFactory
import org.specs2.matcher.ShouldMatchers

class CoffeeMachineTest extends AnyFlatSpec with ShouldMatchers with MockFactory {

  trait CustomTestEnvironment extends UserServiceComponent with UserRepositoryComponent {
    val userRepository = mock[UserRepository]
    val userService = mock[UserService]

    val m = mockFunction[String, String]
   (userService.authenticate _).expects("pavel", *).returns(true)
  }

  "UserService" should "authenticate user" in new CustomTestEnvironment {
    val res = userService.authenticate("pavel", "pass")
    assert(res === true )
  }

}