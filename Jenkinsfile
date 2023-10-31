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
                checkout scm
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

        // stage('SBT Test') {
        //     steps {
        //         container('play') {
        //             script {
        //                 try {
        //                     sh '''
        //                         sbt clean compile flowLintLib test doc
        //                     '''
        //                 } finally {
        //                     junit allowEmptyResults: true, testResults: '**/target/test-reports/*.xml'
        //                 }
        //             }
        //         }
        //     }
        // }

        stage('SBT Release') {
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
                                git config --global --add safe.directory /home/jenkins/workspace/flowcommerce_api-build_PR-410
                                go run release.go
                            '''
                        }
                    }
                }
            }
        }
    }
}
