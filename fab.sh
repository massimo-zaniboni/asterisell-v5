#!/usr/bin/env bash

pedantic=""
rebuild=0
docker images | grep "asterisell/fab" > /dev/null

if [ "$1" = "--update" ]; then
    rebuild=1
fi

if [ "$1" = "--reinstall" ]; then
    rebuild=1
    pedantic="--no-cache"
fi

if [ $? -ne 0 ]; then
    rebuild=1
fi

if [ "$rebuild" = "1" ]; then
    docker rm asterisell
    docker build $pedantic --build-arg USER_ID=$(id -u) --build-arg GROUP_ID=$(id -g) -t "asterisell/fab" .
    if [ $? -ne 0 ]; then
        exit $?
    fi
fi

rm -r -f cache/* > /dev/null

echo "Start a container using asterisell/fab image."
echo ""
echo "Use: "
echo ""
echo "   exit           : for exiting and removing the container"
echo "   fab help       : for a list of admin commands"
echo ""
echo "Read the Asterisell manual at \"doc/manual/out/manual.html\" for more info."
echo ""
echo "NOTE: this container has a private volume with a copy of Stack libraries."
echo "If you maintain it in stopped state, the same volume will be reused."
echo "If you remove it, or update the image, a new container and private volume will be created,"
echo "and some space on disk will be wasted."
echo "In this case  you can remove not used private volumes with "
echo ""
echo "> docker volume prune"
echo ""

docker ps -a | grep "asterisell" > /dev/null
if [ $? -ne 0 ]; then
    docker run --name asterisell --user $(id -u):$(id -g) -t -i -v `pwd`:/asterisell/. asterisell/fab

else
    docker restart asterisell > /dev/null
    docker attach asterisell
fi
