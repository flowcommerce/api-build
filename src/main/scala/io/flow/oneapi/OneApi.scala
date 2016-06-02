package io.flow.oneapi

import com.bryzek.apidoc.spec.v0.models.Service

case class OneApi(services: Seq[Service]) {

  def process(): Either[Seq[String], Service] = {
    Left(Seq("Not done"))
  }

}
