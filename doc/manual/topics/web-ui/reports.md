# Reports

Each run of a report scheduler, generate a report-set, that is a set of related reports. The administrator can administer Report Set, applying common operations on the whole set.

Reports usually must be reviewed and confirmed from administrator, before being visible to end users. If the calls associated to the reports are changed, the administrator is advised.

Every error in sending a report by email (when configured) is signaled from the application, until the problem is fixed.

## Report definition

  * create a report template,
  * test it,
  * create a report scheduler, linked to the report template,
  * set the first date from which generate the new reports on the report template,
  * use the "Generate" button or report scheduler, for starting the report generation,
  * from this date, new reports will be automatically generated, without the need of any manual intervention

## Permissions and notifications

The administrator can view all reports.

Users can view only reports for which they have the right permissions.

Users and administrators are notified in the call report (on-line), and by email, only for reports for which they are directly responsible.

Scheduled Reports can be generated in a "to review" mode. In this case the administrator is advised that there are reports to review. Then the administrator can check the reports, and if they are all ok, he can confirm the reports. Only in this case the reports are visible to users, and they can be sent to email.

Scheduled Reports can be generated directly as "reviewed", and sent immediately.

Reports permissions, and allowed users, are calculated only when the administrator run the "Confirm Reports" action. If an administrator change later the user permissions, old assigned reports remain visible to users. So the administrator must run again the "Confirm Reports" actions for updating the visible reports. Reports will be visible to new users, and not visible to users with different permissions.

## Organization hierarchy and report generation

Reports can be generated also for every parts of a complex organization hierarchy.

This feature is overkill for the case of simple customers, and it has more sense in case of call reporting inside a big organizations.

If there is an organization hierarchy like A/B/C/D, and there is an User U, with role "responsible", for organization B, then U can view all the calls of B, B/C, and B/C/D.

"U" receives only the reports associated exactly to B, and not reports associated to B/C, and B/C/D. This in order to reduce the number of reports sent to the the user U. In any case a report on B, can contain info on B/C and B/C/D.

## Report time-frame

Reports can be generated on all the calls of a year/month/custom time-frame.

The first date of generation of a report, influences all next run of a report.

If you change the date of last report generation, then next reports will be scheduled according the new settings.

After configuring a report scheduler, you must always run the scheduler one time, in order to activate it, and test it.

After the first run of a report scheduler, next reports will be generated automatically.

## Legal documents and consecutive numbers

Legal documents use consecutive numbers, according the last generated legal document.

If there are not legal documents in the system, the first used number is 1, and not the number on the document template.

For specifying an initial legal number different from 1, or for starting the generation of documents with a new non consecutive legal number, you had to:

  * create a fake legal document of type "Placeholder for Invoice Numeration"
  * set the "Billing Document" flag
  * set the legal number with the previous number you want to use on new (real) generated legal documents
  * set the "legal date" equals to the date of new invoices to generate, so it will be used as last reference number
  * leave blank all other params (also the reference date), because they are not used (the document is a "fake")
  * save
