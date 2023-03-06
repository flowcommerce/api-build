package io.flow.lint.linters

import io.apibuilder.spec.v0.models.{Model, Service, Union}

sealed trait EventType
object EventType {
  case object Deleted extends EventType
  case object Upserted extends EventType
}
object EventModel {
  def apply(model: Model): EventModel = {
    if (model.name.matches("_upserted")) {
      EventModel(model, EventType.Upserted)
    } else if (model.name.matches("_upserted")) {
      EventModel(model, EventType.Deleted)
    } else {
      sys.error(s"Cannot determine event type from model named '${model.name}'")
    }
  }
}

case class EventModel(
                     model: Model,
                     eventType: EventType
                      )

case class EventInstance(
  union: Union,
  models: Seq[EventModel]
)

trait EventHelpers extends Helpers {

  private[this] val RegexpTrailingVersion = """\_v\d+$""".r

  private[this] val Suffixes = List(
    "upserted", "deleted"
  )

  def findAllEvents(service: Service): Seq[EventInstance] = {
    service.unions.filter(isEvent).map { union =>
      EventInstance(
        union = union,
        models = union.types.map { t =>
          EventModel(
            service.models.find(_.name == t.`type`).getOrElse {
              sys.error(s"Union '${union.name}': Failed to find model named ${t.`type`}")
            }
          )
        }
      )
    }
  }

  private[this] def isEvent(union: Union): Boolean = {
    val trimmed = RegexpTrailingVersion.replaceAllIn(union.name, "")
    Suffixes.exists { s => trimmed.endsWith(s"_$s") }
  }

}
