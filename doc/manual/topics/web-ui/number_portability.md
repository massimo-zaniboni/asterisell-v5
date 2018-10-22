# Number portability

This table contains the mapping between telephone numbers and ported network operators, and it is used during rating for recognizing the real network operator of the external telephone number, and applying the correct rate.

If this info is directly present on the CDR, then this table can be left empty, because the CDR importer can complete directly this information during the rating phase. 

Usually this table is maintained automatically by a job scheduled to import the updated information from the main network operator used for routing the calls. 

## Multiple porting

A telephone number can be ported to different network operators more than one time. For each time the number is ported, the application will store the ported date. For each call processed by the rating engine, the applicable porting network operator is identified according to the call date.

## Manual input 

The "create" button can be used to manually add mapping information if required.

The CSV file should not contain a header line.

The CSV file may contain duplicate mapped telephone numbers. In this case the numbers are not managed as ported telephone numbers or the telephone number(s) was ported back to its original network operator.

The CSV file may contain repeated lines. They will not generate new entries in the mapping table.

The importing procedure will recognize and merge entries with the same source and dest telephone number, but dates are treated slightly different,  maintaining only older or newer dates, according the specific case.
