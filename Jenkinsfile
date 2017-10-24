pipeline {
  agent any

  /* https://jenkins.io/blog/2017/02/15/declarative-notifications/ */
  stages {
    stage('Start') {
      steps {
        mail (to: 'peterbecich@gmail.com',
              subject: "Job '${env.JOB_NAME}' (${env.BUILD_NUMBER}) started",
              body: "Please go to ${env.BUILD_URL}.");
      }
    }
    stage('Build') {
      steps {
        echo 'Building..'
        sh 'stack upgrade'
        sh 'stack setup'
        sh 'stack build'
      }
    }
    stage('Test') {
      steps {
        echo 'Testing..'
	sh 'stack test'
      }
    }
  }

  post {
    success {
      emailext (
          subject: "SUCCESSFUL: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]'",
          body: """<p>SUCCESSFUL: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]':</p>
            <p>Check console output at &QUOT;<a href='${env.BUILD_URL}'>${env.JOB_NAME} [${env.BUILD_NUMBER}]</a>&QUOT;</p>""",
	  to: "peterbecich@gmail.com"
        )
    }

    failure {
      emailext (
          subject: "FAILED: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]'",
          body: """<p>FAILED: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]':</p>
            <p>Check console output at &QUOT;<a href='${env.BUILD_URL}'>${env.JOB_NAME} [${env.BUILD_NUMBER}]</a>&QUOT;</p>""",
	  to: "peterbecich@gmail.com"
        )
    }
  }
}