package io.flow.stream

import io.apibuilder.validation.ApibuilderType

case class KinesisStream(streamName: String, capturedEvents: Seq[CapturedType])

case class CapturedType(fieldName: String, modelType: ApibuilderType, upsertedDiscriminator: String, deletedDiscriminator: String, deletedHasModel: Boolean)

