#! /bin/bash

#
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

# Tac this, and create a simple graphic for the red and green nightlies
source /home/olepor/dotfiles/scripts/qa-report-gitlab-util.sh

cat <<EOF
# Mender QA - <Month>

## Closed issues this month:

${CLOSED_ISSUES}

## Rejected issues this month:

${REJECTED_ISSUES}

## New issues this month:

${NEW_ISSUES}

## GitLab Nightlies:

$(gitlab-nightly-stats)

## GitLab Nightlies failing tests:

$(gitlab-pipeline-trouble-statistics)

EOF
