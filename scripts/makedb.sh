#!/bin/bash

#
# Build a DB starting from Symfony Schema.
# This script is for developers.
#

BASE=`basename $PWD`

# NOTE: use php54 because php56 is not compatible for some Symfony tasks

if [ "$BASE" = "scripts" ]; then
  cd ..
  php54 symfony propel:build --all-classes
  php54 symfony propel:build-model
  php54 symfony propel:build-sql
  php54 symfony propel:build-forms
  php54 symfony propel:build-filters
  php54 symfony cache:clear
  php54 symfony plugin:publish-assets

  # For supporting recent versions of MySQL
  sed -i -e 's/Type=/Engine=/g' data/sql/lib.model.schema.sql

  # Add character set and collation, because it is not generated from Propel
  sed -i -e 's/Engine=InnoDB/ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin/g' data/sql/lib.model.schema.sql

  # Switch from TEXT (max 64kb) to MEDIUMTEXT (max 16MB) all TEXT fields startinng with medium_text
  sed -i -e 's/mediumtext_\([a-z_]*\)` TEXT /mediumtext_\1` MEDIUMTEXT /' data/sql/lib.model.schema.sql

  # Fix forms of tables having a primaryKey different from `id`
  # NOTE: the work-around is not sufficient
  sed -i -e "s/'telephone_number'[ ]*=> new sfWidgetFormInputHidden/'telephone_number' => new sfWidgetFormInputText/g" lib/form/base/BaseArWholesaleNumberForm.class.php
  sed -i -e "s/'from_date'[ ]*=> new sfWidgetFormInputHidden/'from_date' => new sfWidgetFormDateTime/g" lib/form/base/BaseArWholesaleNumberForm.class.php
 
else
  echo "Execute inside scripts directory."
fi
