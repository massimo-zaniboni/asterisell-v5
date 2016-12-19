Asterisell-5.6-beta
===================

Switched to GPL License.

Asterisell Management Utility is simplified a lot, but now supports only instannces on Docker containers.

IMPORTANT: It is in beta status because there are is no enough experience on the behaviour
under Docker containers, and the management code changed a lot.

Application manual switched to Python Sphinx format.

Various minor improvements.

Asterisell-5.5
==============

Bug fixes and improvements:

* improved error messages in case of bad CSV files
* rating code is more robust in the usage of resources like file handles and database connections
* support of Digitel vendor

Asterisell-5.4
==============

Bug fixes and improvements:
* recognize and signal in a better way, if there are malformed settings in the customers/organizations
* added a command for deleting bad configured customers
* recognize and signal in a better way, if there are missing export-code, for extensions exported to resellers
* added a command for completing missing export-code
* a report scheduler can be configured for not sending and generating reports without a cost
* improved error messages
* for every error message, show the affected CDRs, so errors can be solved by priority
* various minor bug fixes and improvements to the user interface
* minor improvements to the workflow of reports

This version fix a serious bug related to rating of Services. In case you are using services, you must:
* open exactly one service on Services->Services menu,
* change its name adding one character like "a" in the name, for forcing a modification, and save it,
* editing again, removing the "a" and saving again for restoring the correct status.

Doing this, Services will be inspected again, and the correct meta-info will be added to them.

Asterisell-5.3-beta
===================

Minor improvements in the user interface.

Solved some known problems, discovered during using in production:

* #1857
* #1859
* #1857

Asterisell-5.2-beta
===================

Runtime environment uses a more performant:

* Nginx server instead of Apache
* PHP 5.6 instead of PHP 5.3
* various bug-fixes

Asterisell-5.1-beta-8504ad0
===========================

Improvements:

* It is possible adding a prefix to generated invoices numbers.
* Improved the online-help about the generation of invoices starting from a specific number.

Various bug-fixes.

Improved the application manual, but only the online version at https://support.asterisell.com/projects/public-asterisell/wiki