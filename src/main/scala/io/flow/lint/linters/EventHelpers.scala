package io.flow.lint.linters

import io.apibuilder.spec.v0.models.{Model, Service, Union}

object EventModel {
  def fromModel(model: Model): Option[EventModel] = {
    val i = model.name.indexOf("_upserted")
    if (i > 0) {
      Some(UpsertedEventModel(model, model.name.substring(0, i)))
    } else {
      val i = model.name.indexOf("_deleted")
      if (i > 0) {
        Some(DeletedEventModel(model, model.name.substring(0, i)))
      } else {
        None
      }
    }
  }
}

sealed trait EventModel {
  def model: Model
  // eg. for user_upserted -> 'user'
  def prefix: String
}
case class UpsertedEventModel(model: Model, prefix: String) extends EventModel
case class DeletedEventModel(model: Model, prefix: String) extends EventModel

case class EventInstance(
  union: Union,
  models: Seq[EventModel]
) {
  val upserted: Seq[UpsertedEventModel] = models.collect { case m: UpsertedEventModel => m }
  val deleted: Seq[DeletedEventModel] = models.collect { case m: DeletedEventModel => m }
}

trait EventHelpers extends Helpers {

  def findAllEvents(service: Service): Seq[EventInstance] = {
    service.unions.filter(isEvent).map { union =>
      EventInstance(
        union = union,
        models = union.types.flatMap { t =>
          EventModel.fromModel(
            service.models.find(_.name == t.`type`).getOrElse {
              sys.error(s"Union '${union.name}': Failed to find model named ${t.`type`}")
            }
          )
        }
      )
    }
  }

  private[this] def isEvent(union: Union): Boolean = union.name.endsWith("_event")

}
