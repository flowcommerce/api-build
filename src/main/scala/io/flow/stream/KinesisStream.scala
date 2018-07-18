package io.flow.stream

import io.apibuilder.spec.v0.models.Model

case class KinesisStream(streamName: String, capturedEvents: Seq[CapturedType])

case class CapturedType(fieldName: String, modelType: Model, upsertedDiscriminator: String, deletedDiscriminator: String, deletedHasModel: Boolean)

