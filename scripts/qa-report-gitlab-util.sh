#
#
# Utility functions for gathering Nightly statistics from Gitlab mender-qa
#
#
# TODO - should probably get a lot nicer interface for these functions!
# TODO - test stats should also mark which suite it's coming from (integration/acceptance/closed/open-source)

# Get all the scheduled pipelines from the last month
function gitlab-get-nightlies() {
    local -r PREVIOUS_MONTH=$(date --date "$(date +%Y-%m-01) -1 day" +%Y-%m)
    local -r THIS_MONTH=$(date --date "$(date +%Y-%m-01)" +%Y-%m)
    curl --header "PRIVATE-TOKEN: $(pass show private/gitlab/access-token)" "https://gitlab.com/api/v4/projects/12501706/pipelines.json?per_page=100&updated_after=${PREVIOUS_MONTH}-01&updated_before=${THIS_MONTH}-01&source=schedule"
}

# Get all the scheduled pipelines this month so far
function gitlab-get-nightlies-this-month-so-far() {
    # local -r PREVIOUS_MONTH=$(date --date "$(date +%Y-%m-01) -1 day" +%Y-%m)
    local -r TODAY=$(date +%Y-%m-%d)
    local -r THIS_MONTH=$(date --date "$(date +%Y-%m-01)" +%Y-%m)
    curl --header "PRIVATE-TOKEN: $(pass show private/gitlab/access-token)" "https://gitlab.com/api/v4/projects/12501706/pipelines.json?per_page=100&updated_after=${THIS_MONTH}-01&updated_before=${TODAY}&source=schedule"
}

function gitlab-nightlies-get() {
    local -r NIGHTLIES="$1"
    local -r key="$2"
    echo $NIGHTLIES | jq ".[] | ${key}"
}

# TODO - should be used i nget-pipeline-errors-or-failures instead of direct
# curl call
function get-gitlab-pipeline-test-report() {
    [[ $# -ne 1 ]] && return 1
    local -r GITLAB_PIPELINE_ID="$1"
    curl --fail --header "PRIVATE-TOKEN: $(pass show private/gitlab/access-token)" "https://gitlab.com/api/v4/projects/12501706/pipelines/${GITLAB_PIPELINE_ID}/test_report"
}

function get-pipeline-errors-or-failures() {
    echo >&2 "Extracting the failures from the failing pipeline ID's"
    local -r pipeline_ids=$1
    local failing_tests=()
    for pipeline_id in $(echo $1); do
        local GITLAB_PIPELINE_ID="$pipeline_id"
        echo >&2 "Extracting the statistics from:"
        echo >&2 "GITLAB_PIPELINE_ID: ${GITLAB_PIPELINE_ID}"

        curl --fail --header "PRIVATE-TOKEN: $(pass show private/gitlab/access-token)" "https://gitlab.com/api/v4/projects/12501706/pipelines/${GITLAB_PIPELINE_ID}/test_report" | jq '.test_suites | .[] | .test_cases | .[] | select(.status=="error" or .status=="failed") | .name'

        sleep 1
    done
}

function gitlab-pipeline-trouble-statistics() {
    NIGHTLIES="$(gitlab-get-nightlies)"
    PIPELINE_IDS=$(gitlab-nightlies-get "${NIGHTLIES}" .id)
    get-pipeline-errors-or-failures ${PIPELINE_IDS} | \
    awk '
{
        failures[$1]++
}
END {
    for (name in failures) {
            printf("%s=%d\n",name, failures[name])
    }
}
' | sort -t= --key=2 --numeric-sort --reverse
}


function gitlab-nightly-stats() {

    # Get the stats
    NIGHTLIES=$(gitlab-get-nightlies | jq -j '.[] | .created_at," ",.status,"\n"' | tac)

    #
    # Per day overview
    #
    echo -e "|Day\t| Status|"
    echo "|--------|-------|"
    OIFS=${IFS}
    IFS=$'\n'
    for day in ${NIGHTLIES}; do
        # Show the nightly day, and the status
        echo "Day: ${day}"
        # echo -n "|"
        # echo -e -n $(echo -n "${day}" | cut -d' ' -f1 | awk -F'-' '{print $3}' | awk -FT '{print $1}') "\t"
        # echo -n "|"
        # echo " ${day}" | cut -d' ' -f3
        # echo "|"
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
