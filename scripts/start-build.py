import jenkins
import json

with open("/home/olepor/.secrets/jenkins_secrets.sh") as f:
    try:
        secrets = json.loads(f.read())
    except FileNotFoundError:
        print ("Secrets file not found!")

server = jenkins.Jenkins("https://mender-jenkins.mender.io/", username=secrets["JENKINS_UNAME"], password=secrets["JENKINS_PW"])

print(server.jobs_count())

# server.build_job('api_test', )

queue_info = server.get_queue_info()

# print (queue_info[1])

info = server.get_info()

print(info)

jobs = server.get_jobs()

print("jobs:")
print(jobs)

print(type(jobs))
# mender_builder_job = jobs["mender-builder"]

all_jobs = server.get_all_jobs()
print(all_jobs)

print(server.build_job_url("mender-builder"))

# running_builds = server.get_running_builds()

# print(running_builds)

with open("jenkins-mender-builder.json") as f:
    build_data = json.loads(f.read())

print(build_data)

# Set the parameters (e.g. the pull/number/head references) in the dictionary that you want to build with.
# TODO - a simple cli parser needed!

# Currently works to start a job!
queue_index = server.build_job("mender-builder", parameters=build_data)
