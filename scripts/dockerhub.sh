if [ "$TRAVIS_BRANCH" == "production" ]; then
    echo "push to DockerHub"
    docker login -u "$DOCKER_USERNAME" -p "$DOCKER_PASSWORD"
    echo "logged into Docker Hub"
    echo "docker push peterbecich/stock-collector-stock-collector"
    docker push peterbecich/stock-collector-stock-collector
fi
