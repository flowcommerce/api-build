[![Build Status](https://travis-ci.org/flowcommerce/api-build.png?branch=master)](https://travis-ci.org/flowcommerce/api-build)

# api-build

Runs a set of tests against an API defined in apibuilder to ensure
consistency, and then builds a few artifacts used in the end to end
pipeline of API at gilt.

Main features:

  - lint: Automated build to enforce standard conventions at Flow
  - oneapi: Merge multiple API specs into a single API, managed at https://app.apibuilder.io/flow/api
  - proxy: Generates configuration files for the API proxy, routing paths to services

## Installation

  curl https://s3.amazonaws.com/io.flow.aws-s3-public/util/api-build/api-build.jar > ~/api-build.jar

## Examples:

```
  java -jar ~/api-build.jar lint flow/common flow/user
  java -jar ~/api-build.jar oneapi flow/common flow/user
  java -jar ~/api-build.jar build flow/common flow/user
```

Or run the full build:

```
  java -jar ~/api-build.jar all flow/common flow/user
```

## running locally

api-build needs to access apibuilder and requires an API Token:

  1. Goto https://app.apibuilder.io/tokens/ and create a token

  2. Create the ~/.apibuilder/config file - see https://github.com/apicollective/apibuilder-cli


## publishing jar file

We are using one-jar to publish (see https://github.com/sbt/sbt-onejar)

    sbt

    one-jar

## Running from the command line:

    java -jar /web/api-build/target/scala-2.11/api-build_2.11-0.0.1-one-jar.jar lint flow/common flow/user
    java -jar /web/api-build/target/scala-2.11/api-build_2.11-0.0.1-one-jar.jar oneapi flow/common flow/user
    java -jar /web/api-build/target/scala-2.11/api-build_2.11-0.0.1-one-jar.jar all flow/common flow/user

To specify a specific API Builder Profile:

    APIBUILDER_PROFILE=xxx java -jar /web/api-build/target/scala-2.11/api-build_2.11-0.0.1-one-jar.jar all flow/common flow/user

Or to specify a specific APIBUILDER URL and/or Token:

    APIBUILDER_TOKEN=yyy APIBUILDER_API_BASE_URL=http://app.apibuilder.io java -jar /web/api-build/target/scala-2.11/api-build_2.11-0.0.1-one-jar.jar all flow/user

The default behavior is to use the default apibuilder profile.

## Releasing

    go run release.go
