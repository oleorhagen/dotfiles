#! /bin/bash

source /home/olepor/dotfiles/zsh.d/jira-functions.sh

CLOSED_ISSUES_JQL="(labels = QA OR labels = QAInfra ) AND (status = Done) AND (resolved  < startOfMonth() AND resolved > startOfMonth(-1))"

CLOSED_ISSUES_JQL="(labels = QA OR labels = QAInfra ) AND (status != Done) AND (resolved  < startOfMonth() AND resolved > startOfMonth(-1))"

OPENED_ISSUES_JQL="(labels = QA OR labels = QAInfra ) AND (status !=  Done) AND (createdDate > startOfMonth(-1))"

REPORT=$(mktemp)

echo ${MONTHLY_ISSUES_CLOSED} | cat -A

CLOSED_ISSUES=""
IFS=$'\n'
for issue in $(jira-search-query "${CLOSES_ISSUES_JQL}" | jq -j '.issues | .[] | .key, " ", .fields.summary,"\n"'); do
    echo "Issue: ${issue}" | cat -A
    CLOSED_ISSUES="${CLOSED_ISSUES}

* ${issue}
"
done


REJECTED_ISSUES=""
IFS=$'\n'
for issue in $(jira-search-query "${REJECTED_ISSUES_JQL}"| jq -j '.issues | .[] | .key, " ", .fields.summary,"\n"'); do
    echo "Issue: ${issue}" | cat -A
    CLOSED_ISSUES="${REJECTED_ISSUES}

* ${issue}
"
done

NEW_ISSUES=""

for issue in $(jira-search-query "${OPENED_ISSUES_JQL}"| jq -j '.issues | .[] | .key, " ", .fields.summary,"\n"'); do
    echo "Issue: ${issue}" | cat -A
    NEW_ISSUES="${NEW_ISSUES}

* ${issue}
"
done

cat <<EOF
Mender QA

Closed issues this month:

${CLOSED_ISSUES}

Rejected issues this month:

${REJECTED_ISSUES}

New issues this month:

${NEW_ISSUES}




EOF
