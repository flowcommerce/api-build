package io.flow.stream

import io.apibuilder.validation.ApibuilderType

sealed trait EventType {
  def eventName: String
  def typeName: String
  def discriminator: String
}
object EventType {
  case class Upserted(eventName: String, typeName: String, payloadType: ApibuilderType, discriminator: String) extends EventType { override val toString = "upserted" }
  case class Deleted(eventName: String, typeName: String, payloadType: Option[ApibuilderType], discriminator: String) extends EventType { override val toString = "deleted" }
}

