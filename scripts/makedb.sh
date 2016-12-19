#!/bin/bash

#
# Build a DB starting from Symfony Schema.
# This script is for developers.
#

BASE=`basename $PWD`

if [ "$BASE" = "scripts" ]; then
  cd ..
  php symfony propel:build --all-classes
  php symfony propel:build-model
  php symfony propel:build-sql
  php symfony propel:build-forms
  php symfony propel:build-filters
  php symfony cache:clear
  php symfony plugin:publish-assets

  # For supporting recent versions of MySQL
  sed -i -e 's/Type=/Engine=/g' data/sql/lib.model.schema.sql

  # Add character set and collation, because it is not generated from Propel
  sed -i -e 's/Engine=InnoDB/Engine=tokudb COMPRESSION=tokudb_quicklz,DEFAULT CHARACTER SET = utf8, DEFAULT COLLATE = utf8_bin/g' data/sql/lib.model.schema.sql

else
  echo "Execute inside development_tools directory."
fi
