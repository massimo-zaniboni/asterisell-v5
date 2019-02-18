/*  SPDX-License-Identifier: GPL-3.0-or-later */
/* Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com> */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "call_flow_merger.h"
#include "import_pincodes.h"


/////////////////////////////
// INPUT PARSING FUNCTIONS //
/////////////////////////////

int parse_rate_file(const char *);

//////////
// MAIN //
//////////

void show_help() {
    char * cmd = "call_flow_merge";

    printf("\nUsage:\n");
    printf("\n\n\t%s help", cmd);
    printf("\n\n\t%s unit-test", cmd);
    printf("\n\n\t%s calldate itcenter_fccn VOIP-DOMAIN [debug DEBUG-FILE-NAME | nodebug] [--sbc | --mgw | --ipbx] FILE-NAME",cmd);
    printf("\n\n\t%s merge MIN-CALLDATE MAX-CALLDATE MERGE-WINDOW itcenter_fccn VOIP-DOMAIN [debug DEBUG-FILE-NAME | nodebug] [ --pincodes FILE-NAME] [--create FILE-NAME | --append FILE-NAME] [--sbc | --mgw | --ipbx] FILE-NAME [ --sbc | ... ] FILE-NAME ...",cmd);

    printf("\n");

    printf("\n\nVOIP-DOMAIN : something like `voip.ua.pt`");
    printf("\n\nMIN-CALLDATE : the date of CDRs from wich rate (comprehensive) `\"YYYY-MM-DD hh:mm:ss\"`");
    printf("\n\nMAX-CALLDATE : NULL for rating all CDRs greather than MIN-CALLDATE, a date (exclusive) for rating CDRs less than it");
    printf("\n\nDEBUG-FILE-NAME : the name of a HTML file with the debug info");
    printf("\n\nMERGE-WINDOW : load CDRs with a calldate less than specified hours, or greather than specified hours, and see if they can merge with CDRs in the timeframe, and produce the result CDR only if it is inside the specified MIN-CALLDATE and MAX-CALLDATE. In other words part of the call flow chain of a CDR can be outside the MIN and MAX calldate.");

    printf("\n");
    printf("\nThe calldate command return in a single line something like \"MAX-CALL-DATE,MIN-CALL-DATE,TOT-LINES,LINES-WITH-ERRORS,LINE-WITH-ERROR-1:FIELD_WITH_ERROR,...\"");

    printf("\n");
}

/**
 * @brief testIfFails
 * @param t
 * @param d
 * @return 1 if the test fail, 0 if the test is correct.
 */
int testIfFails(int t, char *d) {
    if (t) {
        return 0;
    } else {
        fprintf(stderr, "\nTest fail: %s", d);
        return 1;
    }
}

/**
 * @brief unitTest
 * @return 0 if tests are ok
 */
int unitTest() {
    int fails = 0;

    const int buffSize = 1024;
    char buff[buffSize];
    time_t t1, t2, t3, t4, t5, t6, t7, t8;

    //
    // Test time conversions
    //

    char * d1 = "2014-04-01 10:11:12";
    t1 = unixTimeFromMysqlString(d1);
    strftime(buff, buffSize, "%Y-%m-%d %H:%M:%S", gmtime(& t1));
    t2 = unixTimeFromMysqlString(buff);
    fails += testIfFails(t1 == t2, "Failed time conversion test 1");


    //
    // Test PINCODES
    //
    // TODO test different dates and best and good support

    char * pincode_filename = "/tmp/pincodes-test.dat";
    FILE * f = fopen(pincode_filename, "w");
    fails += testIfFails(f != NULL, "Failed opening file.");

    if (f == NULL) {
        return fails;
    }

    // extension, pincod, date
    // 6000 => 400 2014-02-12 16:05:26
    // 6001 => 700 2014-02-12 16:05:26
    //             *t2
    // 5127 => 500 2014-02-16 21:15:55
    // 6100 => 400 2015-02-12 16:05:26
    //             *t4
    // 6200 => 400 2016-02-12 16:05:26
    //             *t5
    fputs("6000\n400\n2014-02-12 16:05:26\n6001\n700\n2014-02-12 16:05:26\n5127\n500\n2014-02-16 21:15:55\n6100\n400\n2015-02-12 16:05:26\n6200\n400\n2016-02-12 16:05:26", f);

    fclose(f);

    importPinCodes(pincode_filename);
    pinCodesInfo_show();
    printf("\n");

    PinCodeInfo * info;
    t1 = unixTimeFromMysqlString("2013-01-12 16:05:26");
    t2 = unixTimeFromMysqlString("2014-02-13 16:05:26");
    t3 = unixTimeFromMysqlString("2014-04-12 16:05:26");
    t4 = unixTimeFromMysqlString("2015-04-12 16:05:26");
    t5 = unixTimeFromMysqlString("2016-04-12 16:05:26");
    t6 = unixTimeFromMysqlString("2015-02-12 16:05:26");
    t7 = unixTimeFromMysqlString("2016-02-12 16:05:26");
    t8 = unixTimeFromMysqlString("2015-02-12 16:05:26");

    info = pinCodesInfo_get("400", t1);
    fails += testIfFails((info != NULL && strcmp(info->extension, "6000") == 0), "pincodes 1");

    info = pinCodesInfo_get("400", t2);
    fails += testIfFails((info != NULL && strcmp(info->extension, "6000") == 0), "pincodes 2a");
    info = pinCodesInfo_get("700", t2);
    fails += testIfFails((info != NULL && strcmp(info->extension, "6001") == 0), "pincodes 2b");

    info = pinCodesInfo_get("400", t3);
    fails += testIfFails((info != NULL && strcmp(info->extension, "6000") == 0), "pincodes 3");

    info = pinCodesInfo_get("400", t4);
    fails += testIfFails((info != NULL && strcmp(info->extension, "6100") == 0), "pincodes 4");

    info = pinCodesInfo_get("400", t5);
    fails += testIfFails((info != NULL && strcmp(info->extension, "6200") == 0), "pincodes 5");

    info = pinCodesInfo_get("400", t6);
    fails += testIfFails((info != NULL && strcmp(info->extension, "6100") == 0), "pincodes 6");

    info = pinCodesInfo_get("500", t1);
    fails += testIfFails((info != NULL && strcmp(info->extension, "5127") == 0), "pincodes 7");

    info = pinCodesInfo_get("400", t7);
    fails += testIfFails((info != NULL && strcmp(info->extension, "6200") == 0), "pincodes 6");

    //
    // Merge Tests
    //

    fails += merge_tests();


    //
    // Return result
    //

    return fails;

}

int main(int argc, char **argv)
{
    // Set the timezone to UTC, because otherwise
    // the time conversions are not reliable.
    // The CSV log dates contains dates in local time zone,
    // but they should be processed from C code as if they
    // are in UTC, so they are not transformed/converted.

    setenv("TZ", "", 1);
    tzset();

    if (argc > 1) {

        int mergeWindowInHours = 0;

        ParseCommand parseCommand;
        RateParams rateParams;

        int inputParam = 1;
        char * cmd = argv[inputParam++];

        if (strcmp(cmd, "help") == 0) {
            show_help();
            exit(0);
        }

        if (strcmp(cmd, "unit-test") == 0) {
            int r = unitTest();
            printf("\nTest fails: %i\n", r);
            exit(r);
        }

        if (argc > 3) {
            if (strcmp(cmd, "merge") == 0) {
                parseCommand = MERGE_CMD;

                if (argc < 6) {
                    fprintf( stderr, "\nMissing start and end date and merge window");
                    exit(1);
                }

                char * fromDateStr = argv[inputParam++];
                char * toDateStr = argv[inputParam++];

                rateParams.fromDate = unixTimeFromMysqlString(fromDateStr);

                if (strcmp(toDateStr, "NULL") == 0) {
                    rateParams.thereIsToDate = 0;
                } else {
                    rateParams.thereIsToDate = 1;
                    rateParams.toDate = unixTimeFromMysqlString(toDateStr);
                }

                mergeWindowInHours = atoi(argv[inputParam++]);

                rateParams.mergeWindowInSeconds = mergeWindowInHours * 60 * 60;

                RateParams_completeDerivedFields(& rateParams);

            } else if (strcmp(cmd, "calldate") == 0) {
                parseCommand = CALLDATE_CMD;
            } else {
                fprintf( stderr, "\nUnknown command \"%s\"", cmd);
                exit(1);
            }

            char * mode = argv[inputParam++];

            if (strcmp(mode, "itcenter_fccn") == 0) {
                itc_main(argc - inputParam, argv + inputParam, parseCommand, rateParams);
            } else {
                fprintf( stderr, "\nUnknown CSV processing method for vendor \"%s\"", mode);
                exit(1);
            }

        } else {
            fprintf( stderr, "\nMissing expected params.\n");
            exit(1);
        }
    } else {
        show_help();
        exit(1);
    }

    return 0;
}

