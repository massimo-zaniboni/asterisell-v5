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
  sed -i -e 's/Engine=InnoDB/ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin/g' data/sql/lib.model.schema.sql

  # Fix forms of tables having a primaryKey different from `id`
  # NOTE: the work-around is not sufficient
  sed -i -e "s/'telephone_number'[ ]*=> new sfWidgetFormInputHidden/'telephone_number' => new sfWidgetFormInputText/g" lib/form/base/BaseArWholesaleNumberForm.class.php
  sed -i -e "s/'from_date'[ ]*=> new sfWidgetFormInputHidden/'from_date' => new sfWidgetFormDateTime/g" lib/form/base/BaseArWholesaleNumberForm.class.php
 

else
  echo "Execute inside scripts directory."
fi
