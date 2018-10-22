# Reports problems

## Totals on reports

The total cost of vendor reports does not match the grand total on the
report

Check in the online call report if you have some internal vendors with
costs. Internal vendors are not showed on reports

## Regeneration of many reports in the past

|I'm generating many reports in the past, setting the scheduler to 4
months in the past, but only the first set is generated

Asterisell checks for new reports to generate not at every passage of
the cron processor, but it wait some time.

In the JobLog you can see how much it is post-poned.

So if you wait some time, another batch of reports will be executed.

