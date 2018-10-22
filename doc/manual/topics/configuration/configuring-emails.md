# Configuring emails to customers

Customers can receive scheduled reports by email. Mails body and reports can be fully customized.

## Testing mode

During initial testing you can set in the [instances-configuration-file] the option
`send_emails_to_these_users_instead_of_original_receiver`, for
redirecting all the emails originally directed to customers to an
internal test email.

## Adding payment terms and notes to invoices

You can customize the notes, payments terms, and so on, added inside
invoices, in the `Params -> Params` Web form of the application.

## Email Message and Attachment Name

You can customize the message of the emails containing invoices and
other reports:

  - select the report template used for generating them, in the `Reports
    -> All Reports`
  - in the email section of a report, you can customize/specify the
    messages to use in the email containing the report

You can customize email messages both in the scheduler Web form, or you
can customize the message also for a single specific report/invoice.

## Warning emails for high call costs

You can customize
`apps/asterisell/lib/jobs/checks/GenerateMailWarningCustomerForHighCallCost.php`
in case you are sending emails to customers for high call costs.

