package io.flow.lint

trait Helpers {

  val healthcheckResourceTemplate = """
    {
      "type": "io.flow.common.v0.models.healthcheck",
      "plural": "healthchecks",
      "operations": [
        {
          "method": "%s",
          "path": "%s",
          "parameters": [],
          "responses": [
            {
              "code": {
                "integer": {
                  "value": %s
                }
              },
              "type": "%s"
            }
          ]
        }
      ]
    }
  """

  val healthcheck = ServiceBuilder().addResource(
    healthcheckResourceTemplate.format("GET", "/_internal_/healthcheck", 200, "io.flow.common.v0.models.healthcheck")
  )

}
