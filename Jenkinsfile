pipeline {
    agent {
        kubernetes {
            inheritFrom 'default'
            containerTemplates([
                containerTemplate(name: 'play', image: 'flowdocker/play_builder:latest-java17-jammy', command: 'cat', ttyEnabled: true),
            ])
        }
    }

    options {
        disableConcurrentBuilds()
    }

    stages {
        stage('Checkout') {
            steps {
                checkoutWithTags scm
            }
        }

        stage('Tag new version') {
            when { branch 'main' }
            steps {
                script {
                    VERSION = new flowSemver().calculateSemver()
                    new flowSemver().commitSemver(VERSION)
                }
            }
        }

        stage('SBT Test') {
            steps {
                container('play') {
                    script {
                        try {
                            sh '''
                                sbt clean compile test scalafmtSbtCheck scalafmtCheck doc assembly
                            '''
                        } finally {
                            junit allowEmptyResults: true, testResults: '**/target/test-reports/*.xml'
                        }
                    }
                }
            }
        }

        stage('SBT Publish') {
            when { branch 'main' }
            steps {
                container('play') {
                    withCredentials([
                        usernamePassword(
                            credentialsId: 'jenkins-x-github',
                            usernameVariable: 'GIT_USERNAME',
                            passwordVariable: 'GIT_PASSWORD'
                        )
                    ]) {
                        script {
                            sh '''
                                git config --global credential.helper "store --file=/tmp/git-credentials"
                                echo "https://$GIT_USERNAME:$GIT_PASSWORD@github.com" > /tmp/git-credentials
                                git config --global --add safe.directory /home/jenkins/workspace
                                git clone https://github.com/flowcommerce/aws-s3-public.git aws-s3-public
                            '''
                            sh '''
                                cd aws-s3-public
                                git checkout main &&
                                git pull --rebase &&
                                git fetch --tags origin
                                ls
                                pwd
                            '''
                            sh '''
                                git config --global --add safe.directory /home/jenkins/workspace/flowcommerce_api-build_main
                                sbt scalafmtSbtCheck scalafmtCheck clean assembly
                                cp ./target/scala-2.13/api-build-assembly-*.jar ./aws-s3-public/util/api-build/
                                cp ./target/scala-2.13/api-build-assembly-*.jar ./aws-s3-public/util/api-build/api-build.jar
                            '''
                            sh '''
                                cd aws-s3-public
                                git add util/api-build/*
                                git commit -m 'Add new version of api-build' util/api-build
                                git push origin main
                                aws s3 sync util s3://io.flow.aws-s3-public/util --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
                            '''
                            syncDependencyLibrary()
                        }
                    }
                }
            }
        }
    }
}
