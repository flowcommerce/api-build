pipeline {
    agent {
        kubernetes {
            inheritFrom 'kaniko-slim'
            containerTemplates([
                containerTemplate(name: 'play', image: '479720515435.dkr.ecr.us-east-1.amazonaws.com/flowcommerce/play_builder_java17_noble:latest', command: 'cat', ttyEnabled: true),
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
                                sbt clean coverage compile test scalafmtSbtCheck scalafmtCheck doc assembly
                                sbt coverageAggregate
                            '''
                        } finally {
                            junit allowEmptyResults: true, testResults: '**/target/test-reports/*.xml'
                            step([$class: 'ScoveragePublisher', reportDir: 'target/scala-2.13/scoverage-report', reportFile: 'scoverage.xml'])
                            publishHTML (target : [allowMissing: false,
                             alwaysLinkToLastBuild: true,
                             keepAll: true,
                             reportDir: 'target/scala-2.13/scoverage-report',
                             reportFiles: 'index.html',
                             reportName: 'Scoverage Code Coverage',
                             reportTitles: 'Scoverage Code Coverage'])
                        }
                    }
                }
            }
        }

        stage('SBT Publish') {
            when { branch 'main' }
            steps {
                container('play') {
                    withAWS(roleAccount: '479720515435', role: 'flow-prod-eks-production-jenkins-role') {   
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
                                    sbt scalafmtSbtCheck scalafmtCheck clean assembly
                                    cp ./target/scala-2.13/api-build-assembly-*.jar ./aws-s3-public/util/api-build/
                                    cp ./target/scala-2.13/api-build-assembly-*.jar ./aws-s3-public/util/api-build/api-build.jar
                                '''
                                sh '''
                                    cd aws-s3-public
                                    git add util/api-build/*
                                    if git diff --cached --exit-code --quiet
                                    then
                                            echo 'Nothing to commit, not git-pushing nor s3-syncing.' >&2
                                    else
                                            git commit -m 'Add new version of api-build' util/api-build
                                            git push origin main
                                            aws s3 sync util s3://io.flow.aws-s3-public/util
                                    fi
                                '''
                                syncDependencyLibrary()
                            }
                        }
                    }
                }
            }
        }
    }
}
