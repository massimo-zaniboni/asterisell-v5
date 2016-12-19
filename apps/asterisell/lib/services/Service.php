<?php

/* $2013:
*
* Copyright (C) 2013 Massimo Zaniboni,
* <massimo.zaniboni@asterisell.com>,
* Italian Fiscal Code: ZNBMSM74L01F257Z.
* All Rights Reserved.
*
* This SOFTWARE PRODUCT can be used, modified, distributed according the
* terms and conditions of its license. A copy of its license is inside
* the file with name LICENSE, inside the root directory of this SOFTWARE
* PRODUCT.
*
* This SOFTWARE PRODUCT is distributed in the hope that it will be
* useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the specific
* license for more details.
* $
*/

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * A service is always accessible to other jobs.
 * It is usually accessed using single-tone pattern.
 */
class Service
{
}
