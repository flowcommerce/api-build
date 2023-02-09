package io.flow.stream

import io.apibuilder.spec.v0.models.{Field, Model}

sealed trait EventType {
  def eventName: String
  def typeName: String
  def discriminator: String
}
object EventType {
  case class Upserted(eventName: String, typeName: String, fieldName: String, payloadType: Model, idField: Field, discriminator: String) extends EventType { override val toString = "upserted" }
  case class Deleted(eventName: String, typeName: String, payloadType: Option[Model], idField: Field, discriminator: String) extends EventType { override val toString = "deleted" }
}

