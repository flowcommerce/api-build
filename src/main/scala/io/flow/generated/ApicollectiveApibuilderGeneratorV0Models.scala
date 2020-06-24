/**
 * Generated by API Builder - https://www.apibuilder.io
 * Service version: 0.15.4
 * apibuilder 0.15.6 app.apibuilder.io/apicollective/apibuilder-generator/latest/play_2_x_standalone_json
 */
package io.apibuilder.generator.v0.models {

  /**
   * An attribute represents a key/value pair that is optionally used to provide
   * additional instructions / data to the code generator. An example could be an
   * attribute to specify the root import path for a go client..
   */
  final case class Attribute(
    name: String,
    value: String
  )

  /**
   * @param code Machine readable code for this specific error message
   * @param message Description of the error
   */
  final case class Error(
    code: String,
    message: String
  )

  /**
   * Represents a source file
   *
   * @param name The recommended name for the file.
   * @param dir The recommended directory path for the file where appropriate.
   * @param contents The actual source code.
   */
  final case class File(
    name: String,
    dir: _root_.scala.Option[String] = None,
    contents: String,
    flags: _root_.scala.Option[Seq[io.apibuilder.generator.v0.models.FileFlag]] = None
  )

  /**
   * The generator metadata.
   *
   * @param language A comma separate list of the programming language(s) that this generator
   *        produces
   * @param attributes The list of attributes that this code generator can use. You can find the full
   *        list of available attributes and their descriptions at
   *        http://apibuilder.io/doc/attributes
   */
  final case class Generator(
    key: String,
    name: String,
    language: _root_.scala.Option[String] = None,
    description: _root_.scala.Option[String] = None,
    attributes: Seq[String] = Nil
  )

  final case class Healthcheck(
    status: String
  )

  /**
   * The result of invoking a generator.
   *
   * @param source The actual source code.
   * @param files A collection of source files
   */
  final case class Invocation(
    @deprecated("Use files instead") source: String,
    files: Seq[io.apibuilder.generator.v0.models.File]
  )

  /**
   * The invocation form is the payload send to the code generators when requesting
   * generation of client code.
   */
  final case class InvocationForm(
    service: io.apibuilder.spec.v0.models.Service,
    attributes: Seq[io.apibuilder.generator.v0.models.Attribute] = Nil,
    userAgent: _root_.scala.Option[String] = None,
    importedServices: _root_.scala.Option[Seq[io.apibuilder.spec.v0.models.Service]] = None
  )

  /**
   * Allows generator authors to flag files with special characteristics. It is up to
   * the client (i.e. the cli) to decide how to interpret them.
   */
  sealed trait FileFlag extends _root_.scala.Product with _root_.scala.Serializable

  object FileFlag {

    /**
     * Indicates files that an end user starts from but should edit. Not intended to be
     * the final product (see:
     * https://stackoverflow.com/questions/235018/what-is-scaffolding-is-it-a-term-for-a-particular-platform).
     * Consider not overwriting these files when code is re-generated.
     */
    case object Scaffolding extends FileFlag { override def toString = "scaffolding" }

    /**
     * UNDEFINED captures values that are sent either in error or
     * that were added by the server after this library was
     * generated. We want to make it easy and obvious for users of
     * this library to handle this case gracefully.
     *
     * We use all CAPS for the variable name to avoid collisions
     * with the camel cased values above.
     */
    final case class UNDEFINED(override val toString: String) extends FileFlag

    /**
     * all returns a list of all the valid, known values. We use
     * lower case to avoid collisions with the camel cased values
     * above.
     */
    val all: scala.List[FileFlag] = scala.List(Scaffolding)

    private[this]
    val byName: Map[String, FileFlag] = all.map(x => x.toString.toLowerCase -> x).toMap

    def apply(value: String): FileFlag = fromString(value).getOrElse(UNDEFINED(value))

    def fromString(value: String): _root_.scala.Option[FileFlag] = byName.get(value.toLowerCase)

  }

}

package io.apibuilder.generator.v0.models {

  package object json {
    import play.api.libs.json.__
    import play.api.libs.json.JsString
    import play.api.libs.json.Writes
    import play.api.libs.functional.syntax._
    import io.apibuilder.common.v0.models.json._
    import io.apibuilder.generator.v0.models.json._
    import io.apibuilder.spec.v0.models.json._

    private[v0] implicit val jsonReadsUUID = __.read[String].map { str =>
      _root_.java.util.UUID.fromString(str)
    }

    private[v0] implicit val jsonWritesUUID = new Writes[_root_.java.util.UUID] {
      def writes(x: _root_.java.util.UUID) = JsString(x.toString)
    }

    private[v0] implicit val jsonReadsJodaDateTime = __.read[String].map { str =>
      _root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseDateTime(str)
    }

    private[v0] implicit val jsonWritesJodaDateTime = new Writes[_root_.org.joda.time.DateTime] {
      def writes(x: _root_.org.joda.time.DateTime) = {
        JsString(_root_.org.joda.time.format.ISODateTimeFormat.dateTime.print(x))
      }
    }

    private[v0] implicit val jsonReadsJodaLocalDate = __.read[String].map { str =>
      _root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseLocalDate(str)
    }

    private[v0] implicit val jsonWritesJodaLocalDate = new Writes[_root_.org.joda.time.LocalDate] {
      def writes(x: _root_.org.joda.time.LocalDate) = {
        JsString(_root_.org.joda.time.format.ISODateTimeFormat.date.print(x))
      }
    }

    implicit val jsonReadsApibuilderGeneratorFileFlag = new play.api.libs.json.Reads[io.apibuilder.generator.v0.models.FileFlag] {
      def reads(js: play.api.libs.json.JsValue): play.api.libs.json.JsResult[io.apibuilder.generator.v0.models.FileFlag] = {
        js match {
          case v: play.api.libs.json.JsString => play.api.libs.json.JsSuccess(io.apibuilder.generator.v0.models.FileFlag(v.value))
          case _ => {
            (js \ "value").validate[String] match {
              case play.api.libs.json.JsSuccess(v, _) => play.api.libs.json.JsSuccess(io.apibuilder.generator.v0.models.FileFlag(v))
              case err: play.api.libs.json.JsError =>
                (js \ "file_flag").validate[String] match {
                  case play.api.libs.json.JsSuccess(v, _) => play.api.libs.json.JsSuccess(io.apibuilder.generator.v0.models.FileFlag(v))
                  case err: play.api.libs.json.JsError => err
                }
            }
          }
        }
      }
    }

    def jsonWritesApibuilderGeneratorFileFlag(obj: io.apibuilder.generator.v0.models.FileFlag) = {
      play.api.libs.json.JsString(obj.toString)
    }

    def jsObjectFileFlag(obj: io.apibuilder.generator.v0.models.FileFlag) = {
      play.api.libs.json.Json.obj("value" -> play.api.libs.json.JsString(obj.toString))
    }

    implicit def jsonWritesApibuilderGeneratorFileFlag: play.api.libs.json.Writes[FileFlag] = {
      new play.api.libs.json.Writes[io.apibuilder.generator.v0.models.FileFlag] {
        def writes(obj: io.apibuilder.generator.v0.models.FileFlag) = {
          jsonWritesApibuilderGeneratorFileFlag(obj)
        }
      }
    }

    implicit def jsonReadsApibuilderGeneratorAttribute: play.api.libs.json.Reads[Attribute] = {
      for {
        name <- (__ \ "name").read[String]
        value <- (__ \ "value").read[String]
      } yield Attribute(name, value)
    }

    def jsObjectAttribute(obj: io.apibuilder.generator.v0.models.Attribute): play.api.libs.json.JsObject = {
      play.api.libs.json.Json.obj(
        "name" -> play.api.libs.json.JsString(obj.name),
        "value" -> play.api.libs.json.JsString(obj.value)
      )
    }

    implicit def jsonWritesApibuilderGeneratorAttribute: play.api.libs.json.Writes[Attribute] = {
      new play.api.libs.json.Writes[io.apibuilder.generator.v0.models.Attribute] {
        def writes(obj: io.apibuilder.generator.v0.models.Attribute) = {
          jsObjectAttribute(obj)
        }
      }
    }

    implicit def jsonReadsApibuilderGeneratorError: play.api.libs.json.Reads[Error] = {
      for {
        code <- (__ \ "code").read[String]
        message <- (__ \ "message").read[String]
      } yield Error(code, message)
    }

    def jsObjectError(obj: io.apibuilder.generator.v0.models.Error): play.api.libs.json.JsObject = {
      play.api.libs.json.Json.obj(
        "code" -> play.api.libs.json.JsString(obj.code),
        "message" -> play.api.libs.json.JsString(obj.message)
      )
    }

    implicit def jsonWritesApibuilderGeneratorError: play.api.libs.json.Writes[Error] = {
      new play.api.libs.json.Writes[io.apibuilder.generator.v0.models.Error] {
        def writes(obj: io.apibuilder.generator.v0.models.Error) = {
          jsObjectError(obj)
        }
      }
    }

    implicit def jsonReadsApibuilderGeneratorFile: play.api.libs.json.Reads[File] = {
      for {
        name <- (__ \ "name").read[String]
        dir <- (__ \ "dir").readNullable[String]
        contents <- (__ \ "contents").read[String]
        flags <- (__ \ "flags").readNullable[Seq[io.apibuilder.generator.v0.models.FileFlag]]
      } yield File(name, dir, contents, flags)
    }

    def jsObjectFile(obj: io.apibuilder.generator.v0.models.File): play.api.libs.json.JsObject = {
      play.api.libs.json.Json.obj(
        "name" -> play.api.libs.json.JsString(obj.name),
        "contents" -> play.api.libs.json.JsString(obj.contents)
      ) ++ (obj.dir match {
        case None => play.api.libs.json.Json.obj()
        case Some(x) => play.api.libs.json.Json.obj("dir" -> play.api.libs.json.JsString(x))
      }) ++
      (obj.flags match {
        case None => play.api.libs.json.Json.obj()
        case Some(x) => play.api.libs.json.Json.obj("flags" -> play.api.libs.json.Json.toJson(x))
      })
    }

    implicit def jsonWritesApibuilderGeneratorFile: play.api.libs.json.Writes[File] = {
      new play.api.libs.json.Writes[io.apibuilder.generator.v0.models.File] {
        def writes(obj: io.apibuilder.generator.v0.models.File) = {
          jsObjectFile(obj)
        }
      }
    }

    implicit def jsonReadsApibuilderGeneratorGenerator: play.api.libs.json.Reads[Generator] = {
      for {
        key <- (__ \ "key").read[String]
        name <- (__ \ "name").read[String]
        language <- (__ \ "language").readNullable[String]
        description <- (__ \ "description").readNullable[String]
        attributes <- (__ \ "attributes").read[Seq[String]]
      } yield Generator(key, name, language, description, attributes)
    }

    def jsObjectGenerator(obj: io.apibuilder.generator.v0.models.Generator): play.api.libs.json.JsObject = {
      play.api.libs.json.Json.obj(
        "key" -> play.api.libs.json.JsString(obj.key),
        "name" -> play.api.libs.json.JsString(obj.name),
        "attributes" -> play.api.libs.json.Json.toJson(obj.attributes)
      ) ++ (obj.language match {
        case None => play.api.libs.json.Json.obj()
        case Some(x) => play.api.libs.json.Json.obj("language" -> play.api.libs.json.JsString(x))
      }) ++
      (obj.description match {
        case None => play.api.libs.json.Json.obj()
        case Some(x) => play.api.libs.json.Json.obj("description" -> play.api.libs.json.JsString(x))
      })
    }

    implicit def jsonWritesApibuilderGeneratorGenerator: play.api.libs.json.Writes[Generator] = {
      new play.api.libs.json.Writes[io.apibuilder.generator.v0.models.Generator] {
        def writes(obj: io.apibuilder.generator.v0.models.Generator) = {
          jsObjectGenerator(obj)
        }
      }
    }

    implicit def jsonReadsApibuilderGeneratorHealthcheck: play.api.libs.json.Reads[Healthcheck] = {
      (__ \ "status").read[String].map { x => new Healthcheck(status = x) }
    }

    def jsObjectHealthcheck(obj: io.apibuilder.generator.v0.models.Healthcheck): play.api.libs.json.JsObject = {
      play.api.libs.json.Json.obj(
        "status" -> play.api.libs.json.JsString(obj.status)
      )
    }

    implicit def jsonWritesApibuilderGeneratorHealthcheck: play.api.libs.json.Writes[Healthcheck] = {
      new play.api.libs.json.Writes[io.apibuilder.generator.v0.models.Healthcheck] {
        def writes(obj: io.apibuilder.generator.v0.models.Healthcheck) = {
          jsObjectHealthcheck(obj)
        }
      }
    }

    implicit def jsonReadsApibuilderGeneratorInvocation: play.api.libs.json.Reads[Invocation] = {
      for {
        source <- (__ \ "source").read[String]
        files <- (__ \ "files").read[Seq[io.apibuilder.generator.v0.models.File]]
      } yield Invocation(source, files)
    }

    def jsObjectInvocation(obj: io.apibuilder.generator.v0.models.Invocation): play.api.libs.json.JsObject = {
      play.api.libs.json.Json.obj(
        "source" -> play.api.libs.json.JsString(obj.source),
        "files" -> play.api.libs.json.Json.toJson(obj.files)
      )
    }

    implicit def jsonWritesApibuilderGeneratorInvocation: play.api.libs.json.Writes[Invocation] = {
      new play.api.libs.json.Writes[io.apibuilder.generator.v0.models.Invocation] {
        def writes(obj: io.apibuilder.generator.v0.models.Invocation) = {
          jsObjectInvocation(obj)
        }
      }
    }

    implicit def jsonReadsApibuilderGeneratorInvocationForm: play.api.libs.json.Reads[InvocationForm] = {
      for {
        service <- (__ \ "service").read[io.apibuilder.spec.v0.models.Service]
        attributes <- (__ \ "attributes").read[Seq[io.apibuilder.generator.v0.models.Attribute]]
        userAgent <- (__ \ "user_agent").readNullable[String]
        importedServices <- (__ \ "imported_services").readNullable[Seq[io.apibuilder.spec.v0.models.Service]]
      } yield InvocationForm(service, attributes, userAgent, importedServices)
    }

    def jsObjectInvocationForm(obj: io.apibuilder.generator.v0.models.InvocationForm): play.api.libs.json.JsObject = {
      play.api.libs.json.Json.obj(
        "service" -> io.apibuilder.spec.v0.models.json.jsObjectService(obj.service),
        "attributes" -> play.api.libs.json.Json.toJson(obj.attributes)
      ) ++ (obj.userAgent match {
        case None => play.api.libs.json.Json.obj()
        case Some(x) => play.api.libs.json.Json.obj("user_agent" -> play.api.libs.json.JsString(x))
      }) ++
      (obj.importedServices match {
        case None => play.api.libs.json.Json.obj()
        case Some(x) => play.api.libs.json.Json.obj("imported_services" -> play.api.libs.json.Json.toJson(x))
      })
    }

    implicit def jsonWritesApibuilderGeneratorInvocationForm: play.api.libs.json.Writes[InvocationForm] = {
      new play.api.libs.json.Writes[io.apibuilder.generator.v0.models.InvocationForm] {
        def writes(obj: io.apibuilder.generator.v0.models.InvocationForm) = {
          jsObjectInvocationForm(obj)
        }
      }
    }
  }
}

