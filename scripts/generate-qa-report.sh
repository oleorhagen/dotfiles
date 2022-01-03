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

# Get all the scheduled pipelines from the last month
function gitlab-get-nightlies() {
    local -r PREVIOUS_MONTH=$(date --date "$(date +%Y-%m-01) -1 day" +%Y-%m)
    local -r THIS_MONTH=$(date --date "$(date +%Y-%m-01)" +%Y-%m)
    curl --header "PRIVATE-TOKEN: $(pass show private/gitlab/access-token)" "https://gitlab.com/api/v4/projects/12501706/pipelines.json?per_page=100&updated_after=${PREVIOUS_MONTH}-01&updated_before=${THIS_MONTH}-01&source=schedule"

}


function gitlab-nightly-stats() {

    # Get the stats
    NIGHTLIES=$(gitlab-get-nightlies | jq -j '.[] | .created_at," ",.status,"\n"' | tac)

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

    local -r NR_OF_DAYS_IN_PREVIOUS_MONTH=$(date --date "$(date +%Y-%m-01) -1 day" +%d)

    #
    # Stats
    #
    echo
    echo "### Simple statistics"
    echo
    echo "${NIGHTLIES}" | awk -v DAYS=${NR_OF_DAYS_IN_PREVIOUS_MONTH} \
'
$2 == "failed" {failed += 1}
$2 == "success" {passed += 1}
END {
print "passed: " passed
print "failed: " failed
print "Success ratio: " passed/DAYS
print "Failure ratio: " failed/DAYS

}'
}

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

EOF
