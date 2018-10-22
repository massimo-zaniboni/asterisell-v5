#!/usr/bin/env bash

c=$(find topics/ -type f -newer out.chk | wc --lines)
if [ "$c" != "0" ]
then
       rm -r -f out/*

       /opt/ditac/bin/ditac \
         -format webhelp5 \
         -p chain-pages both \
         -images img \
         -p xsl-resources-directory res \
         out/_.html \
         manual.ditamap && touch out.chk
fi

