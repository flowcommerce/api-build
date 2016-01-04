# api-lint
Runs a set of tests against an API defined in apidoc to ensure consistency

## running locally

api-lint needs to access apidoc and requires an API Token:

1. Goto http://apidoc.me/tokens/ and create a token

2. Create the ~/.apidoc/config file - see https://github.com/mbryzek/apidoc-cli

3. run script/sbt which will pull your information from
~/.apidoc/config or otherwise specify env variables from command line:

    APIDOC_API_TOKEN=xxx sbt

If you are running apidoc locally, also specify the host:

    APIDOC_API_TOKEN=xxx APIDOC_API_HOST=http://api.apidoc.me sbt
