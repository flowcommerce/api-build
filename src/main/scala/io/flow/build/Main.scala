package io.flow.build

import io.apibuilder.spec.v0.models.Service
import io.flow.{oneapi, lint, proxy, stream}

object Main extends App {

  import scala.concurrent.ExecutionContext.Implicits.global

  private[this] var globalErrors = scala.collection.mutable.ListBuffer[String]()

  private[this] def controllers(buildType: BuildType): Seq[Controller] = {
    val all = scala.collection.mutable.ListBuffer[Controller]()
    all.append(lint.Controller())
    all.append(stream.Controller())
    if (buildType.oneapi) {
      all.append(oneapi.Controller())
    }
    if (buildType.proxy) {
      all.append(proxy.Controller())
    }
    all
  }

  ApibuilderConfig.load() match {
    case Left(error) => {
      println(s"** Error loading apibuilder config: $error")
      System.exit(1)
    }

    case Right(profile) => {
      args.toList match {
        case Nil => {
          println(s"** ERROR: Specify type[${BuildType.all.mkString(", ")}] and command[lint|oneapi|all]")
        }

        case one :: Nil => {
          println(s"** ERROR: Specify type[${BuildType.all.mkString(", ")}] and command[lint|oneapi|all]")
        }

        case typ :: command :: rest => {
          BuildType.fromString(typ) match {
            case None => {
              println(s"** ERROR: Invalid buildType[$typ]. Must be one of: " + BuildType.all.mkString(", "))
            }

            case Some(buildType) => {
              val selected = if (command == "all") { controllers(buildType) } else { controllers(buildType).filter(_.command == command) }
              selected.toList match {
                case Nil => {
                  println(s"** ERROR: Invalid command[$command]. Must be one of: all, " + controllers(buildType).map(_.command).mkString(", "))
                }

                case _ => {
                  Downloader.withClient(profile) { dl =>
                    val services = rest.flatMap { name =>
                      Application.parse(name) match {
                        case None => {
                          globalErrors += s"Could not parse application[$name]"
                          None
                        }

                        case Some(app) => {
                          dl.service(app) match {
                            case Left(error) => {
                              globalErrors += s"Failed to download app[${app.label}]: $error"
                              None
                            }
                            case Right(service) => {
                              Some(service)
                            }
                          }
                        }
                      }
                    }
                    run(buildType, dl, selected, services)
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  private[this] def run(buildType: BuildType, downloader: Downloader, controllers: Seq[Controller], services: Seq[Service]) {

    var errors = scala.collection.mutable.Map[String, Seq[String]]()
    if (!globalErrors.isEmpty) {
      errors += ("config" -> globalErrors)
    }

    controllers.foreach { controller =>
      println("==================================================")
      println(s"${controller.name} Starting")
      println("==================================================")

      controller.run(buildType, downloader, services)
      controller.errors.foreach {
        case (key, errs) => {
          errors.get(key) match {
            case None => {
              errors.put(key, errs)
            }
            case Some(existing) => {
              errors.put(key, existing ++ errs)
            }
          }
        }
      }
    }

    println("")
    errors.size match {
      case 0 => println(s"SUMMARY: NO ERRORS")
      case 1 => println(s"SUMMARY: 1 ERROR")
      case n => println(s"SUMMARY: $n ERRORS")
    }
    errors.keys.toSeq.sorted foreach { key =>
      println(key)
      errors(key).foreach { err =>
        println(s"  - $err")
      }
      println("")
    }
    println("")
    System.exit(errors.size)
  }

}
