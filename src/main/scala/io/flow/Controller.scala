package io.flow.build

trait Controller {

  private[this] var errors = scala.collection.mutable.Map[String, Seq[String]]()
  private[this] val GlobalError = "Global"

  protected[this] def addError(organization: String, application: String, error: String) {
    addError(s"$organization/$application", error)
  }

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

  /**
    * Run things and return a list of errors
    */
  def run(
    args: Seq[String]
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  )

  def errors(): Map[String, Seq[String]] = errors.toMap

}
