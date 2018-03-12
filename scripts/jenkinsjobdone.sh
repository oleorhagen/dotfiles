#!/bin/bash
JOBNR=$1
BUILD_URL="https://mender-jenkins.mender.io/job/mender-builder/${JOBNR}"
while true;
do
    sleep 10
    RES=$(http --auth ${JENKINS_UNAME}:${JENKINS_PW} --auth-type basic https://mender-jenkins.mender.io/job/mender-builder/${JOBNR}/consoleText | grep -E 'Finished: SUCCESS|FAILURE')
    if [ ${#RES} -gt 0 ]; then
         if [[ ${RES}='Finished: SUCCESS' ]]; then
             SUCCESS_STRING="jenkins-job ${JOBNR} finished!
 SUCCESS!!!
:D"
             notify-send -i '/home/olepor/dotfiles/scripts/jenkins-success.svg' -u critical "Jenkins-build done!" "${SUCCESS_STRING}"
             $(http PUT ${SLACKWEBHOOKURL} name=${JENKINS_UNAME} text="${SUCCESS_STRING}
<${BUILD_URL}>" username="jenkins-message" icon_emoji=':burn:')

        else
            FAILURE_STRING="jenkins-job ${JOBNR} finished!\n FAILURE!!!\n :("
            notify-send -i '/home/olepor/dotfiles/scripts/fire-jenkins.svg' -u critical "Jenkins-build done!" "${FAILURE_STRING}"
            http PUT ${SLACKWEBHOOKURL} name=${JENKINS_UNAME} text="${FAILURE_STRING}" username="jenkins-message" icon_emoji=':burn:'
        fi
        exit 0
    fi
done
