.. _Asterisell: https://www.asterisell.com
.. _support: support@asterisell.com
.. _assistance: support@asterisell.com

Asterisell Usage
================

.. important::
   Each page of the application has an extensive online help. This manual will not repeat this information.

Recurring concepts
------------------

Hierarchical structure definitions (tree-like) are preferred respect flat definitions (list-like):

- Customers are defined using hierarchical structures, for example "Acme Organization/Accounting Office/John Smith".
- Rating rules are defined using hierarchical structures, for example "acme-vendor-telecom/outgoing/emergency-number".

The advantage of hierarchical structures are:

- common parameters can be set in the parent node, and they are inherited in the children nodes, that can eventually override them;
- complex structures can be named referring only to the proper parent node;

New information does not replace old information, overwriting it, but it is added to the database, specifying when it became active.
So a complete history of the past is preserved. The advantages are:

- changes of rates and services can be planned in the future;
- CDRs of the past can be re-rated, applying the (correct) old rate specifications;
- customers can change price-categories, or subscription of services, without loosing the history of the past;
- services can change prices, but old prices can (optionally and if configured) still applied to old customers;

Asterisell uses a conservative approach about CDR rating. In case of doubt it does not rate a CDR, but it signals the problem.

Recurring work-flow
-------------------

Asterisell run-time workflow is:

- CDRs importing
- CDRs rating
- error reporting
- generation of scheduled reports and invoices
- sending of report and invoices to customers, after the admin reviewed and confirmed them

Asterisell supports fully customizable jobs that can be added in every phase of the workflow.


Official calldate
.................

The official calldate is the date of the last billed CDRs. All CDRs before this date are not re-rated automatically, because they are considerd as already billed to customers. CDRs after this date are re-rated automatically every time configuration params change.

Error notifications
...................

Asterisell generate errors in ``Status -> Current Problems``.

Errors are also signaled by email, generating an error report, that is
sent to all administrators with one or more of these roles:

-  ``Notified for warning errors``
-  ``Notified for errors``
-  ``Notified for critical errors``

Every time you rerate CDRs, the solved errors disappear from the table. In some rare
circumstance, it is better deleting the table and rerating all the CDRs, for seeing
if the problem disappeared or not.

Emergency email
~~~~~~~~~~~~~~~

You can define another administrator account, with a distinct email, and with only
this role ``Notified for critical errors``.
In this way you can be sure that the email is "fired" only in case of
very important problems, and it acts like an emergency email. It can be linked to a pager.

Critical errors are fired in case of:

-  high call costs surpassing the limit of an account of more than two
   time the configured limit (otherwise it is considered a warning)
-  there are too much calls not rated
-  there is a critical erorr, avoiding the processing of CDRs
-  and so on


Asterisell tries to reduce the number of emails notifications, signaling you only when a certain type of error doubled is appareance respect the past signaled email. In any case there can be false allarms, and some noisy.

Accountant
..........

You can generate summary reports in Asterisell to send only to system
administrator with role ``Accountant``.
  
TODO continue
