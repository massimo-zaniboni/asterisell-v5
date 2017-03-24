
This is a Docker container that can execute Symfony development tasks.

The problem is that Symfony development tasks requires an old version of PHP, different
from the PHP version used for running Asterisell. The tools are used only for generating
new versions of the database, and other administration tasks used only during application
development.

I created a Docker image supporting these tasks.

For installing the image (needed only the first time), execute:

    docker build -t "asterisell/symfony_dev" .

The content of current `Dockerfile` will be used for creating the image.

For creating a Docker container go to current Asterisell base directory (very important), and execute

    docker run --rm -t -i -v `pwd`:/asterisell/. asterisell/symfony_dev /bin/bash

This command will associate to container ``/asterisell`` directory, the corresponding Asterisell development
directory on the local host.

Then from inside the container execute:

    cd /asterisell
    rm -r -f cache/*

The cache must be removed because it contains hard-coded info that is not valid inside the container.

NOTE: every operation done on the container ``/asterisell`` directory (but not other directories) are automatically done
on the local filesystem, in the currently installed Asterisell directory.

NOTE: at exit from the shell the container is stopped and removed, due to ``--rm`` option.

Now you can execute inside the container, the development commands. Usually they are of type

    ./symfony
    cd scripts && ./makedb.sh

