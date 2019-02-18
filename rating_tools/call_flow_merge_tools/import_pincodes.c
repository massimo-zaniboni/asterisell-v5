/*  SPDX-License-Identifier: GPL-3.0-or-later */
/* Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com> */

#include "import_pincodes.h"
#include "call_flow_merger.h"

/**
 * @brief pinCodesInfo a JSLI array from PinCode to list of PinCodeInfo,
 * stored as an indexed JLI of pointers of PinCodeInfo.
 */
Pvoid_t pinCodesInfo;

time_t unixTimeFromMysqlString(char * s) {
    struct tm tmlol;
    memset(&tmlol, 0, sizeof(struct tm));

    char * last = (char *) strptime(s, "%Y-%m-%d%t%H:%M:%S", &tmlol);

    if (last == NULL) {
        fprintf( stderr, "Date in bad format \"%s\"", s);
        exit(1);
    }

    time_t t = mktime(&tmlol);

    return t;
}


/**
 * @brief pinCodesInfo_get return best or good enough result.
 * PinCodes can be not perfectly setup, so a call matching an existing PINCODE
 * is good, also if the PINCODE is activated later the call.
 * @param pinCode
 * @return NULL if there is no info, the best matching value otherwise
 */
PinCodeInfo * pinCodesInfo_get(char * pinCode, time_t callDate) {

    if (pinCode == NULL) {
        return NULL;
    }

    PinCodeInfo * perfectResult = NULL;
    PinCodeInfo * goodResult = NULL;

    PWord_t pvalue;
    JSLG(pvalue, pinCodesInfo, (uint8_t *) pinCode);

    if (pvalue == NULL) {
        return NULL;
    }

    Pvoid_t extensions = (Pvoid_t) *pvalue;

    Word_t index1 = 0;
    int isThereInfo = 1;

    while (isThereInfo) {
        index1++;  // 1 is the first index in a Judy1
        Word_t pvalue2;
        J1BC(isThereInfo, extensions, index1, pvalue2);

        if (isThereInfo) {
            PinCodeInfo * info = (PinCodeInfo *) pvalue2;

            if (difftime(callDate, info->fromDate) >= 0) {

                if (perfectResult == NULL) {
                    perfectResult = info;
                } else {
                    if (difftime(info->fromDate, perfectResult->fromDate) > 0) {
                        // select most recent change
                        perfectResult = info;
                    }
                }

            } else {
                if (goodResult == NULL) {
                    goodResult = info;
                } else {
                    if (difftime(info->fromDate, goodResult->fromDate) < 0) {
                        // select first value in the history
                        goodResult = info;
                    }
                }

            }
        }
    }

    if (perfectResult == NULL) {
        return goodResult;
    } else {
        return perfectResult;
    }
}

/**
 * @brief pinCodesInfo_show
 */
void pinCodesInfo_show() {
    char index[1024];
    PWord_t pvalue;

    index[0] = '\0';                    // start with smallest string.
    JSLF(pvalue, pinCodesInfo, (uint8_t *) index);       // get first string
    while (pvalue != NULL)
    {
        Pvoid_t extensions = (Pvoid_t) *pvalue;

        printf("\nPincode %s", index);

        Word_t index1 = 0;
        int isThereInfo = 1;

        while (isThereInfo) {
            index1++;  // 1 is the first index in a Judy1
            Word_t pvalue2;
            J1BC(isThereInfo, extensions, index1, pvalue2);

            if (isThereInfo) {
                PinCodeInfo * info = (PinCodeInfo *) pvalue2;

                printf("\n\textension: %s ", info->extension);
            }
        }

        JSLN(pvalue, pinCodesInfo, (uint8_t *) index);   // get next string
    }


}

ssize_t getline_withouLF(char **lineptr, size_t *n, FILE *stream) {
    ssize_t r = getline(lineptr, n, stream);
    if (r > 0) {
        if ((*lineptr)[r - 1] == '\n')
            (*lineptr)[r - 1] = '\0';
    }

    return r;
}


/**
 * @brief importPinCodes
 * @param fileName
 * @return -1 in case of error, the number of read records otherwise.
 */
int importPinCodes(char * fileName) {

    FILE * fp;

    pinCodesInfo = NULL;

    fp = fopen(fileName, "r");
    if (fp == NULL) {
        return -1;
    }

    size_t len = 0;
    char * line = (char *) malloc(sizeof(char) * len);
    ssize_t read;
    int count = 0;

    while (1) {
        // format: extension, pincode, mysql date

        read = getline_withouLF(&line, &len, fp);
        if (read == -1) {
            break;
        }

        PinCodeInfo * info = (PinCodeInfo *) malloc(sizeof(PinCodeInfo));

        info->extension = strdup(line);
        info->fromDate = 0;

        read = getline_withouLF(&line, &len, fp);
        if (read == -1) {
            break;
        }

        PPvoid_t pvalue;
        JSLI(pvalue, pinCodesInfo, (uint8_t *) line);

        // Store the associated info as INDEX values of the JudyArray,
        // that in this case acts like a list of ITCSourceCDR.
        int rc;
        J1S(rc, *pvalue, (Word_t) info);

        read = getline_withouLF(&line, &len, fp);
        if (read == -1) {
            break;
        }
        time_t fromDate = unixTimeFromMysqlString(line);
        info->fromDate = fromDate;

        count++;
    }

    free(line);

    return count;
}
