package io.flow.build

import com.bryzek.apidoc.spec.v0.models.Service

trait Controller {

  private[this] var errors = scala.collection.mutable.Map[String, Seq[String]]()
  private[this] val GlobalError = "Global"

  protected[this] def addError(message: String) {
    addError(GlobalError, message)
  }
  
  protected[this] def addError(key: String, error: String) {
    errors.get(key) match {
      case None => {
        errors.put(key, Seq(error))
      }
      case Some(existing) => {
        errors.put(key, existing ++ Seq(error))
      }
    }
  }

  def name: String

  def command: String

  /**
    * Run things and return a list of errors
    */
  def run(
    buildType: BuildType,
    downloader: Downloader,
    services: Seq[Service]
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  )

  def errors(): Map[String, Seq[String]] = errors.toMap

}
