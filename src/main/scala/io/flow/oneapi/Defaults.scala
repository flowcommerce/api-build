package io.flow.oneapi

object Defaults {

  val FieldDescriptions = Map(
    "id" -> "Globally unique identifier",
    "number" -> "Client's unique identifier for this object",
    "organization" -> "Refers to your organization's account identifier"
  )

  val ParameterDescriptions = Map(
    "id" -> "Filter by one or more IDs of this resource",
    "limit" -> "The maximum number of results to return",
    "offset" -> "The number of results to skip before returning results",
    "organization" -> "Refers to your organization's account identifier"
  )

  val ResponseDescriptions = Map(
    "200" -> "Successful response",
    "201" -> "Operation succeeded and the resource was created",
    "204" -> "Operation succeeded. No content is returned",
    "401" -> "Authorization failed",
    "404" -> "Resource was not found",
    "422" -> "One or more errors were found with the data sent in the request. The body of the response contains specific details on what data failed validation."
  )

}
