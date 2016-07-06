[![Build Status](https://travis-ci.org/flowcommerce/api-lint.png?branch=master)](https://travis-ci.org/flowcommerce/api-lint)

# api-build

Runs a set of tests against an API defined in apidoc to ensure
consistency, and then builds a few artifacts used in the end to end
pipeline of API at gilt.

Main features:

  - lint: Automated build to enforce standard conventions at Flow
  - oneapi: Merge multiple API specs into a single API, managed at apidoc.me/flow/api
  - proxy: Generates configuration files for the API proxy, routing paths to services

## Installation

  curl https://s3.amazonaws.com/io.flow.aws-s3-public/util/api-build/api-build.jar > ~/api-build.jar

## Examples:

```
  java -jar ~/api-lint.jar lint flow/common flow/user
  java -jar ~/api-lint.jar oneapi flow/common flow/user
  java -jar ~/api-lint.jar build flow/common flow/user
```

Or run the full build:

```
  java -jar ~/api-lint.jar all flow/common flow/user
```

## running locally

api-build needs to access apidoc and requires an API Token:

  1. Goto http://apidoc.me/tokens/ and create a token

  2. Create the ~/.apidoc/config file - see https://github.com/mbryzek/apidoc-cli


## publishing jar file

We are using one-jar to publish (see https://github.com/sbt/sbt-onejar)

    sbt

    one-jar

## Running from the command line:

    java -jar /web/api-build/target/scala-2.11/api-build_2.11-0.0.1-one-jar.jar lint flow/common flow/user
    java -jar /web/api-build/target/scala-2.11/api-build_2.11-0.0.1-one-jar.jar oneapi flow/common flow/user
    java -jar /web/api-build/target/scala-2.11/api-build_2.11-0.0.1-one-jar.jar all flow/common flow/user

To specify a specific APIDOC Profile:

    APIDOC_PROFILE=xxx java -jar /web/api-build/target/scala-2.11/api-build_2.11-0.0.1-one-jar.jar all flow/common flow/user

Or to specify a specific APIDOC URL and/or Token:

    APIDOC_API_TOKEN=yyy APIDOC_API_URI=http://api.apidoc.mc java -jar /web/api-build/target/scala-2.11/api-build_2.11-0.0.1-one-jar.jar all flow/user

The default behavior is to use the default apidoc profile.

## Releasing

    go run release.go
