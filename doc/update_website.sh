#!/bin/bash

# Update https://www.asterisell.com website 

RDIR=/srv/www/asterisell_com/www
RHOST=asterisell.com

cp -f website/keybase.txt website/output/.
cd website
rsync --chown=nginx:nginx --delete -avzhe ssh output/ root@$RHOST:$RDIR
cd ..
cd manual
rsync --chown=nginx:nginx --delete -avzhe ssh out/ root@$RHOST:$RDIR/manual/
