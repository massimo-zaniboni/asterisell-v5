/*  SPDX-License-Identifier: GPL-3.0-or-later */
/* Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com> */

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
