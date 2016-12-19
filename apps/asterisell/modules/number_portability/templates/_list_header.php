<?php

/* $LICENSE 2009, 2010:
 *
 * Copyright (C) 2009, 2010 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
 *
 * This file is part of Asterisell.
 *
 * Asterisell is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Asterisell is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
 * $
 */

echo '<br/>';
echo form_tag("number_portability/importCSV", 'multipart=true') . input_file_tag('csvFile', array('size' => 30)) . '      '. submit_tag(__('Import from CSV'), array('name' => 'csv')) . '      '  . submit_tag(__('Import from TWT'), array('name' => 'twt')) . '</form>';
echo '<br/>';
echo '<p>CSV file format is: \'"SOURCE-NUMBER","DEST-NUMBER", "YYYY-MM-DD"\'.</p>';
echo '<p>TWT file format is: \'IGNORE;CALL-TIMESTAMP;IGNORE;CALLED-NUMBER;PROPER PREFIX;IGNORE;IGNORE;IGNORE;IGNORE;IGNORE;IGNORE;IGNORE\'.</p>';
?>