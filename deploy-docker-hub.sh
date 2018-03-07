#!/bin/sh

set -ue

docker load < result
docker tag matrix-gitlab-bot:latest plapadoo/matrix-gitlab-bot:latest
echo "$DOCKER_PASSWORD" | docker login -u "$DOCKER_USERNAME" --password-stdin
docker push plapadoo/matrix-gitlab-bot:latest
