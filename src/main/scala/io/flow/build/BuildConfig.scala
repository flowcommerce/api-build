package io.flow.build

/** Additional configuration passed to the run method of each Controller.
  *
  * @param protocol
  *   Used by the proxy controller when constructing the value of each host in the servers list.
  * @param domain
  *   Used by the proxy controller when constructing the value of each host in the servers list.
  * @param output
  *   Where controllers write files created.
  */
case class BuildConfig(protocol: String, domain: String, output: java.nio.file.Path)
