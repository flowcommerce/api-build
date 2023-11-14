package io.flow.stream

import io.apibuilder.spec.v0.models.{Enum, Model, Union}

case class StreamDescriptor(streams: Seq[KinesisStream])

case class KinesisStream(
  streamName: String,
  shortName: String,
  capturedEvents: Seq[CapturedType],
  allModels: Seq[Model],
  allUnions: Seq[Union],
  allEnums: Seq[Enum],
)

case class CapturedType(
  fieldName: String,
  typeName: String,
  modelType: Model,
  upsertedDiscriminator: String,
  deletedDiscriminator: String,
  deletedHasModel: Boolean,
)
