#!/bin/bash
JOBNR=$1
while true;
do
    sleep 100
    RES=$(http --auth ${JENKINS_UNAME}:${JENKINS_PW} --auth-type basic https://mender-jenkins.mender.io/job/mender-builder/${JOBNR}/consoleText | grep -E 'Finished: SUCCESS|FAILURE')
    if [ ${#RES} -gt 0 ]; then
         if [[ ${RES}='Finished: SUCCESS' ]]; then
                 notify-send -i '/home/olepor/dotfiles/scripts/jenkins-success.svg' -u critical "Jenkins-build done!" "jenkins-job ${JOBNR} finished!\n SUCCESS!!!\n :D"
        else
            notify-send -i '/home/olepor/dotfiles/scripts/fire-jenkins.svg' -u critical "Jenkins-build done!" "jenkins-job ${JOBNR} finished!\n FAILURE!!!\n :("
        fi
        exit 0
    fi
done
