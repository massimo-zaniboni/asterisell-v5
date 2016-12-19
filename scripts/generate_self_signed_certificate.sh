#!/bin/bash

cd /etc/ssl/certs/
openssl genrsa -des3 -out asterisell_server.key 1024
openssl req -new -key asterisell_server.key -out asterisell_server.csr
cp asterisell_server.key asterisell_server.key.org
openssl rsa -in asterisell_server.key.org -out asterisell_server.key
openssl x509 -req -days 365 -in asterisell_server.csr -signkey asterisell_server.key -out asterisell_server.crt
chown apache asterisell_server.key asterisell_server.crt

echo "Generated self signed certificate file asterisell_server.crt and key asterisell_server.key, in directory /etc/ssl/certs"
