package io.flow.proxy

import com.bryzek.apidoc.spec.v0.models.Service

case class Controller() extends io.flow.build.Controller {

  override val name = "Proxy"
  override val command = "proxy"

  def run(
    services: Seq[Service]
  ) (
    implicit ec: scala.concurrent.ExecutionContext
  ) {
    println("Building proxy from: " + services.map(_.name).mkString(", "))

    val all = services.map { service =>
      ProxyBuilder(service).yaml()
    }.mkString("\n")

    val path = "/tmp/proxy.config"
    writeToFile(path, all)
    System.out.println(s"Proxy configuration written to ${path}")
  }

  private[this] def writeToFile(path: String, contents: String) {
    import java.io.{BufferedWriter, File, FileWriter}

    val bw = new BufferedWriter(new FileWriter(new File(path)))
    try {
      bw.write(contents)
    } finally {
      bw.close()
    }
  }
  
}
