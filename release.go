package main

import (
	"github.com/flowcommerce/tools/executor"
)

func main() {
	executor := executor.Create("api-lint")

	executor = executor.Add("cd ../aws-s3-public\ngit checkout master")
	executor = executor.Add("cd ../aws-s3-public\ngit pull --rebase")
	executor = executor.Add("cd ../aws-s3-public\ngit fetch --tags origin")

	executor = executor.Add("dev tag --label micro")
	executor = executor.Add("sbt clean one-jar")
	executor = executor.Add("cp ./target/scala-2.11/api-lint*-one-jar.jar /web/aws-s3-public/util/api-lint/")
	executor = executor.Add("cp ./target/scala-2.11/api-lint*-one-jar.jar /web/aws-s3-public/util/api-lint/api-lint.jar")

	executor = executor.Add("cd ../aws-s3-public\ngit add util/api-lint/*")
	executor = executor.Add("cd ../aws-s3-public\ngit commit -m 'Add new version of api-lint' util/api-lint")
	executor = executor.Add("cd ../aws-s3-public\ngit push origin master")
	executor = executor.Add("cd ../aws-s3-public\naws s3 sync util s3://io.flow.aws-s3-public/util --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers")

	executor.Run()
}
