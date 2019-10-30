# source ~/.secrets/jenkins_secrets.sh
JSON_STR="{\"parameter\": [{\"name\": \"MENDER_REV\", \"value\": \"pull/322/head\"}]}"

# JSON_STR='{"parameter": [{"name": "MENDER_REV", "value": "pull/322/head"}]}'
BUILD_URL=' https://mender-jenkins.mender.io/job/mender-builder/build'

echo $JSON_STR

curl -X POST ${BUILD_URL} \
     --user $JENKINS_UNAME:$JENKINS_PW \
     --data-urlencode json="${JSON_STR}"
