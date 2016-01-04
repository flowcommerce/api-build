package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}
import scala.collection.JavaConversions._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class LintSpec extends FunSpec with Matchers {

  val Dir = new java.io.File("src/main/scala/io/flow/lint/linters")

  it("tmp") {
    val path = "/tmp/organization.json"
    val contents = scala.io.Source.fromFile(path).getLines.mkString("\n")

    import scala.concurrent.ExecutionContext.Implicits.global

    Await.result(
      Config.apidocApiClient.validations.post(contents),
      Duration(5, "seconds")
    ).service match {
      case None => println("No service")
      case Some(service) => {
        println("Got service")
        println(service.toString)
      }
    }
  }



  it("All lists all linters") {
    // avoid runtime reflection but still fail the build if a new
    // linter is introduced that is not added to the constant All
    // val Pattern = """case object (.+)\s+extends\s+Linter""".r
    val Pattern = """.*case\s*object (.+) extends Linter.*""".r

    for ( file <- Dir.listFiles if file.getName.endsWith(".scala") && !file.getName.endsWith("Helpers.scala") ) {
      var found = false
      scala.io.Source.fromFile(file).getLines.foreach { l =>
        l match {
          case Pattern(className) => {
            found = true
            if (!Lint.All.map(_.toString).contains(className)) {
              fail(s"Lint.All is missing $className - it contains: " + Lint.All.map(_.toString).mkString(", "))
            }
          }
          case _ => {
          }
        }
      }

      if (!found) {
        fail(s"Linter definition not recognized in file[$file]")
      }
    }
  }

  it("Lint.All is alphabetized") {
    Lint.All.map(_.toString).sorted should be(Lint.All.map(_.toString))
  }

}
