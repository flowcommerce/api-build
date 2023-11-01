package io.flow.build

import io.apibuilder.spec.v0.models.Service

trait Controller {

  private[this] val internalErrors = scala.collection.mutable.Map[String, Seq[String]]()
  private[this] val GlobalError = "Global"

  protected[this] def addError(message: String): Unit = {
    addError(GlobalError, message)
  }

  protected[this] def addError(key: String, error: String): Unit = {
    internalErrors.get(key) match {
      case None => {
        internalErrors.put(key, Seq(error))
      }
      case Some(existing) => {
        internalErrors.put(key, existing ++ Seq(error))
      }
    }
    ()
  }

  def name: String

  def command: String

  /** Run things and return a list of errors
    */
  def run(
    buildType: BuildType,
    downloadCache: DownloadCache,
    services: Seq[Service]
  )(implicit
    ec: scala.concurrent.ExecutionContext
  ): Unit

  def errors(): Map[String, Seq[String]] = internalErrors.toMap

}
