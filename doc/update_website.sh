#!/bin/bash

# Update https://www.asterisell.com website 

cp -f website/keybase.txt website/output/.
cd website
rsync --chown=www-data:www-data --delete -avzhe ssh output/ root@asterisell.com:/var/www/www_asterisell_com
cd ..
cd manual
rsync --chown=www-data:www-data --delete -avzhe ssh out/ root@asterisell.com:/var/www/www_asterisell_com/manual/
