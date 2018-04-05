#!/bin/sh

set -ue

REPO_NAME=$(basename "$TRAVIS_REPO_SLUG")

docker load < result
docker tag "$REPO_NAME:latest" "$TRAVIS_REPO_SLUG:latest"
echo "$DOCKER_PASSWORD" | docker login -u "$DOCKER_USERNAME" --password-stdin
docker push "$TRAVIS_REPO_SLUG:latest"
