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
                                go run release.go
                            '''
                        }
                    }
                }
            }
        }
        // stage('Test Release') {
        //     steps {
        //         container('play') {
        //             withCredentials([
        //                 usernamePassword(
        //                     credentialsId: 'jenkins-x-github',
        //                     usernameVariable: 'GIT_USERNAME',
        //                     passwordVariable: 'GIT_PASSWORD'
        //                 )
        //             ]) {
        //                 script {
        //                     sh 'export GIT_USERNAME=$GIT_USERNAME'
        //                     sh 'export GIT_PASSWORD=$GIT_PASSWORD'
        //                     sh 'go run release.go'
        //                 }
        //             }
        //         }
        //     }
        // }
    }
}
