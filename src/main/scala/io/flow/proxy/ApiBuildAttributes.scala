package io.flow.proxy

import io.apibuilder.spec.v0.models.Service
import play.api.libs.json.{JsString, JsValue}

/**
  * Helper to parse the values defined for
  * the attribute named 'api-build'
  */
case class ApiBuildAttributes(services: Seq[Service]) {

  def host(serviceName: String): Option[String] = {
    get(serviceName, "host").map(_.asInstanceOf[JsString].value)
  }

  private[this] def get(serviceName: String, attributeName: String): Option[JsValue] = {
    services.find(_.name.toLowerCase == serviceName.toLowerCase) match {
      case None => None
      case Some(svc) => apiBuildAttributeValue(svc, attributeName)
    }
  }

  private[this] def apiBuildAttributeValue(service: Service, name: String): Option[JsValue] = {
    service.attributes.find(_.name == "api-build") match {
      case None => None
      case Some(attr) => attr.value.value.get(name)
    }
  }
  
}
