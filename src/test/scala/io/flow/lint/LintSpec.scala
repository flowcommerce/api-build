package io.flow.lint

import io.flow.build.BuildType
import org.scalatest.{FunSpec, Matchers}

class LintSpec extends FunSpec with Matchers {

  val Dir = new java.io.File("src/main/scala/io/flow/lint/linters")

  it("All lists all linters") {
    // avoid runtime reflection but still fail the build if a new
    // linter is introduced that is not added to the constant All
    // val Pattern = """case object (.+)\s+extends\s+Linter""".r
    val Pattern = """.*case\s*object (.+) extends Linter.*""".r

    val all = BuildType.all.flatMap(Lint.forBuildType).map(_.toString).distinct.toSet

    for ( file <- Dir.listFiles if file.getName.endsWith(".scala") && !file.getName.endsWith("Helpers.scala") ) {
      var found = false
      scala.io.Source.fromFile(file).getLines.foreach {
        case Pattern(className) => {
          found = true
          if (!all.contains(className)) {
            fail(s"Lint.All is missing linter[$className] - it contains: " + all.toSeq.sorted.mkString(", "))
          }
        }
        case _ => {
        }
      }

      if (!found) {
        fail(s"Linter definition not recognized in file[$file]")
      }
    }
  }

  it("Lint.All is alphabetized") {
    BuildType.all.foreach { bt =>
      Lint.forBuildType(bt).map(_.toString).sorted should be(
        Lint.forBuildType(bt).map(_.toString)
      )
    }
  }

}
