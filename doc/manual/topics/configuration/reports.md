# Reports

Reports are documents of various physical formats (PDF, CSV and so on), to send to customers, or accountants or administrators, and containing invoices or analysis of calls.

Reports are usually scheduled, and not generated manually. Initial configuration is not an easy process, because there are many settings, for different type of usage scenario. In order to simplify the configuration process, Asterisell configure some reports during initial installation, hoping they can be used as a starting-point.

## Scheduled reports

The report workflow is usually this:

  - define a report template, generate it for testing
  - use it as template for a report scheduler:
  - run the first batch of reports, pressing the `Generate Reports` button on the report scheduler
  - all next run of the report scheduler will be executed automatically
  - each report scheduler pass will produce a report-set
  - each report-set contains all the generated reports of the same type, on the same time-frame, for example all the invoices of all customers
  - the admin can review a report set
  - until the admin do not confirm a report-set, the reports are not sent to the customers/users
  - the admin is advised, if after generation the calls associated to the report are modified, and it is obliged to regenerate it

## Billing reports

Billing reports (i.e. legal reports) are important, because they are also
used for determining the official call date: the calls before this call
date are never automatically rerated if there are changes in rating
params, because they are considered as already billed to customers.

In case of changes of rating params, only unbilled calls will be automatically rerated.

When a billing report is confirmed, the official call date is advanced automatically.

Note that a billing report can be also simply an analysis of calls to send to the administrator, and it is not obliged to be a set of invoices.

## Sending of reports

Reports can be sent to proper billable users. If an user has no associated
email, the application will warn the administrator. When the email is
specified, the reports will be sent automatically.

## Adding new reports

You can define new type of reports adding source code under
``apps/asterisell/lib/jobs/reports``, then add them into the Fabric
variable ``custom_reports``, so they can be selected for scheduling and
then generated.

