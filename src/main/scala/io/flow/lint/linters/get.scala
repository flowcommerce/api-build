package io.flow.lint.linters

import io.flow.lint.Linter
import com.bryzek.apidoc.spec.v0.models.Service

case object Get extends Linter {

  override def validate(service: Service): Seq[String] = {
    Nil
  }

}
