


# Get the data needed for the monthly report

function jira-search-query() {

    QUERY=$(cat <<EOF
{
    "jql": "$1",
    "startAt": 0,
    "maxResults": 15,
    "fields": [
        "summary",
        "status",
        "assignee"
    ]
}
EOF
         )

    curl \
        --fail \
        --show-error \
        -u oleorhagen:$(pass show private/tracker.mender.io/oleorhagen) \
        -X POST \
        -H "Content-type: application/json" \
        --data-raw "${QUERY}" https://tracker.mender.io/rest/api/2/search
}
