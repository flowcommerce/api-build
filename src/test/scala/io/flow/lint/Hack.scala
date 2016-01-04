package io.flow.lint

import com.bryzek.apidoc.spec.v0.models._
import org.scalatest.{FunSpec, Matchers}

class Hack extends FunSpec with Matchers {

  it("temporary debugging") {
    Seq("/tmp/carrier.json", "/tmp/organization.json").foreach { path =>
      println("")
      println("")
      println(path)
      Lint.fromFile(path) match {
        case Nil => println("valid")
        case errors => {
          println("1 or more errors:")
          errors.foreach { err =>
            println(s"  - $err")
          }
        }
      }
    }
  }

}
