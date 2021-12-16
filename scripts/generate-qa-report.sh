#! /bin/bash

#
# TODO - Automate the Gitlab pipeline statistics-generator
# TODO - make functions out of the repetetive code here
# TODO - split the gitlab-generator out into a separate file and function
# TODO - gather statistics on naughty and misbehaving tests
#

source /home/olepor/dotfiles/zsh.d/jira-functions.sh

CLOSED_ISSUES_JQL="(labels = QA OR labels = QAInfra ) AND (status = Done) AND (resolved  < startOfMonth() AND resolved > startOfMonth(-1))"

REJECTED_ISSUES_JQL="(labels = QA OR labels = QAInfra ) AND (status = Rejected) AND (resolved  < startOfMonth() AND resolved > startOfMonth(-1))"

OPENED_ISSUES_JQL="(labels = QA OR labels = QAInfra ) AND (status !=  Done) AND (createdDate > startOfMonth(-1) AND createdDate < startOfMonth())"

REPORT=$(mktemp)

CLOSED_ISSUES=""
IFS=$'\n'
for issue in $(jira-search-query "${CLOSED_ISSUES_JQL}" | jq -j '.issues | .[] | .key, " ", .fields.summary,"\n"'); do
    echo >&2 "Issue: ${issue}" | cat -A
    CLOSED_ISSUES="${CLOSED_ISSUES}

* ${issue}
"
done

echo >&2 "Closed issues"
echo >&2 "${CLOSED_ISSUES}"

REJECTED_ISSUES=""
IFS=$'\n'
for issue in $(jira-search-query "${REJECTED_ISSUES_JQL}"| jq -j '.issues | .[] | .key, " ", .fields.summary,"\n"'); do
    echo >&2 "Issue: ${issue}" | cat -A
    REJECTED_ISSUES="${REJECTED_ISSUES}

* ${issue}
"
done

echo >&2 "Rejected issues"
echo >&2 "${REJECTED_ISSUES}"

NEW_ISSUES=""

for issue in $(jira-search-query "${OPENED_ISSUES_JQL}"| jq -j '.issues | .[] | .key, " ", .fields.summary,"\n"'); do
    echo >&2 "Issue: ${issue}" | cat -A
    NEW_ISSUES="${NEW_ISSUES}

* ${issue}
"
done


echo >&2 "New issues"
echo >&2 "${NEW_ISSUES}"


####################
# Gitlab Nightlies #
####################

# JQ filter for Gitlab nightlies:

# jq '.pipelines | .[] | .details.status.text == failed, and created_at regexpequeals 2021-11'
# Filter for processing the Gitlab scheduled nightlies JSON
# jq -j '.pipelines |  map(select(.created_at | test("2021-11.*"))) | .[] | .created_at," ", .details.status.text,"\n"'

# Tac this, and create a simple graphic for the red and green nightlies


function gitlab-nigtly-stats() {

    NIGHTLIES="$(cat ~/page{1,2,3,4}.json | jq -j '.pipelines |  map(select(.created_at | test("2021-11.*"))) | .[] | .created_at," ", .details.status.text,"\n"' | tac)"

    #
    # Per day overview
    #
    echo -e "Day\tStatus"
    echo "---------------"
    OIFS=${IFS}
    IFS=$'\n'
    for day in ${NIGHTLIES}; do
        # Show the nightly day, and the status
        echo -e -n $(echo -n "${day}" | cut -d' ' -f1 | awk -F'-' '{print $3}' | awk -FT '{print $1}') "\t"
        echo " ${day}" | cut -d' ' -f3
        # TODO - add emojis
        # RES=$(echo " ${day}" | cut -d' ' -f3)
        # if [[ "${RES}" = "failed" ]]; then
        #     printf '\U1F534'
        # fi
    done
    IFS=${OIFS}

    #
    # Stats
    #
    echo
    echo "Simple statistics"
    echo
    echo "${NIGHTLIES}" | awk '
$2 == "failed" {failed += 1}
$2 == "passed" {passed += 1}
END {
print "passed: " passed
print "failed: " failed
print "Success ratio: " passed/30
print "Failure ratio: " failed/30

}'
}

cat <<EOF
Mender QA

Closed issues this month:

${CLOSED_ISSUES}

Rejected issues this month:

${REJECTED_ISSUES}

New issues this month:

${NEW_ISSUES}

Gitlab Nightlies:

$(gitlab-nigtly-stats)

EOF
