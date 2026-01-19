package io.flow.stream

import io.apibuilder.spec.v0.models.{Field, Model}

sealed trait EventType {
  def eventName: String
  def typeName: String
  def discriminator: String
}
object EventType {
  sealed trait UpsertLike extends EventType {
    def fieldName: String
    def payloadType: Model
    def idField: Field
  }

  case class Inserted(
    eventName: String,
    typeName: String,
    fieldName: String,
    payloadType: Model,
    idField: Field,
    discriminator: String,
  ) extends UpsertLike

  case class Updated(
    eventName: String,
    typeName: String,
    fieldName: String,
    payloadType: Model,
    idField: Field,
    discriminator: String,
  ) extends UpsertLike

  case class Upserted(
    eventName: String,
    typeName: String,
    fieldName: String,
    payloadType: Model,
    idField: Field,
    discriminator: String,
  ) extends UpsertLike

  case class Deleted(
    eventName: String,
    typeName: String,
    payloadType: Option[Model],
    idField: Field,
    discriminator: String,
  ) extends EventType
}
