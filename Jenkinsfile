pipeline {
	options {
		timeout(time: 90, unit: 'MINUTES')
		buildDiscarder(logRotator(numToKeepStr: (env.BRANCH_NAME == 'master' || env.BRANCH_NAME ==~ 'BETA.*') ? '100':'5', artifactNumToKeepStr: (env.BRANCH_NAME == 'master' || env.BRANCH_NAME ==~ 'BETA.*') ? '15':'2'))
		disableConcurrentBuilds(abortPrevious: true)
		timestamps()
	}
	agent {
		label "ubuntu-latest"
	}
	tools {
		maven 'apache-maven-latest'
		jdk 'openjdk-jdk24-latest'
	}
	stages {
		stage('Build and Test') {
			steps {
					sh """#!/bin/bash -x
					
					java -version
					
					mkdir -p $WORKSPACE/tmp
					
					unset JAVA_TOOL_OPTIONS
					unset _JAVA_OPTIONS
					
					# The max heap should be specified for tycho explicitly
					# via configuration/argLine property in pom.xml
					# export MAVEN_OPTS="-Xmx2G"
					
					mvn clean install -f org.eclipse.jdt.core.compiler.batch -DlocalEcjVersion=99.99 -Dmaven.repo.local=$WORKSPACE/.m2/repository -DcompilerBaselineMode=disable -DcompilerBaselineReplace=none

					# Build and test without DOM-first to ensure no regression takes place
					mvn -U clean verify --batch-mode --fail-at-end -Dmaven.repo.local=$WORKSPACE/.m2/repository \
						-pl !org.eclipse.jdt.core.tests.javac \
						-Ptest-on-javase-24 -Pbree-libs -Papi-check -Pjavadoc -Pp2-repo \
						-Dmaven.test.failure.ignore=true \
						-Dcompare-version-with-baselines.skip=false \
						-Djava.io.tmpdir=$WORKSPACE/tmp -Dproject.build.sourceEncoding=UTF-8 \
						-Dtycho.surefire.argLine="--add-modules ALL-SYSTEM -Dcompliance=1.8,11,17,21,23,24 -Djdt.performance.asserts=disabled" \
						-DDetectVMInstallationsJob.disabled=true \
						-Dtycho.apitools.debug \
						-Dtycho.debug.artifactcomparator \
						-e \
						-Dcbi-ecj-version=99.99
					"""
			}
			post {
				always {
					// The following lines use the newest build on master that did not fail a reference
					// To not fail master build on failed test maven needs to be started with "-Dmaven.test.failure.ignore=true" it will then only marked unstable.
					// To not fail the build also "unstable: true" is used to only mark the build unstable instead of failing when qualityGates are missed
					// Also do not record mavenConsole() as failing tests are logged with ERROR duplicating the failure into the "Maven" plugin
					// To accept unstable builds (test errors or new warnings introduced by third party changes) as reference using "ignoreQualityGate:true"
					// To only show warnings related to the PR on a PR using "publishAllIssues:false"
					// The eclipse compiler name is changed because the logfile not only contains ECJ but also API warnings.
					// "pattern:" is used to collect warnings in dedicated files avoiding output of junit tests treated as warnings   
					junit '**/target/surefire-reports/*.xml'
					//discoverGitReferenceBuild referenceJob: 'eclipse.jdt.core-github/master'
					//recordIssues publishAllIssues:false, ignoreQualityGate:true, tool: eclipse(name: 'Compiler and API Tools', pattern: '**/target/compilelogs/*.xml'), qualityGates: [[threshold: 1, type: 'DELTA', unstable: true]]
				}
			}
		}
		stage('javac specific tests') {
			steps {
				sh """#!/bin/bash -x
					mkdir -p $WORKSPACE/tmp
					
					unset JAVA_TOOL_OPTIONS
					unset _JAVA_OPTIONS
					# force qualifier to start with `z` so we identify it more easily and it always seem more recent than upstrea
					mvn install -DskipTests -Djava.io.tmpdir=$WORKSPACE/tmp -Dmaven.repo.local=$WORKSPACE/.m2/repository \
						-Pbree-libs \
						-Dtycho.buildqualifier.format="'z'yyyyMMdd-HHmm" \
						-Pp2-repo \
						-Djava.io.tmpdir=$WORKSPACE/tmp -Dproject.build.sourceEncoding=UTF-8 \
						-Dcbi-ecj-version=99.99 \
						-pl org.eclipse.jdt.core.compiler.batch,org.eclipse.jdt.core,org.eclipse.jdt.core.javac,org.eclipse.jdt.core.javac.feature,org.eclipse.jdt.core.tests.model,org.eclipse.jdt.core.tests.compiler,repository

					mvn verify --batch-mode -f org.eclipse.jdt.core.tests.javac -Dmaven.repo.local=$WORKSPACE/.m2/repository \
						--fail-at-end -Ptest-on-javase-24 -Pbree-libs \
						-DfailIfNoTests=false -DexcludedGroups=org.junit.Ignore -DproviderHint=junit47 \
						-Papi-check -Djava.io.tmpdir=$WORKSPACE/tmp -Dproject.build.sourceEncoding=UTF-8 \
						-Dmaven.test.failure.ignore=true -Dmaven.test.error.ignore=true  
"""
			}
			post {
				always {
					archiveArtifacts artifacts: '*.log,*/target/work/data/.metadata/*.log,*/tests/target/work/data/.metadata/*.log,apiAnalyzer-workspace/.metadata/*.log,repository/target/repository/**,**/target/artifactcomparison/**', allowEmptyArchive: true
					junit 'org.eclipse.jdt.core.tests.javac/target/surefire-reports/*.xml'
					discoverGitReferenceBuild referenceJob: 'jdt-core-incubator/dom-with-javac'
					//recordIssues ignoreQualityGate:true, tool: junitParser(pattern: 'org.eclipse.jdt.core.tests.javac/target/surefire-reports/*.xml'), qualityGates: [[threshold: 1, type: 'DELTA', unstable: true]]
				}
			}
		}
	}
}
