/* $LICENSE 2014:
 *
 * Copyright (C) 2014 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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

#ifndef __IMPORT_PINCODES_17753__
#define __IMPORT_PINCODES_17753__

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>

#include <malloc.h>

#include <glib.h>

typedef struct {

    char * extension;

    time_t fromDate;

} PinCodeInfo;


/**
 * @brief pinCodesInfo_get return best or good enough result.
 * PinCodes can be not perfectly setup, so a call matching an existing PINCODE
 * is good, also if the PINCODE is activated later the call.
 * @param pinCode
 * @return NULL if there is no info, the best matching value otherwise
 */
PinCodeInfo * pinCodesInfo_get(char * pinCode, time_t callDate);

/**
 * @brief importPinCodes
 * @param fileName
 * @return -1 in case of error, the number of read records otherwise.
 */
int importPinCodes(char * fileName);

void pinCodesInfo_show();

time_t unixTimeFromMysqlString(char * s);

#endif
