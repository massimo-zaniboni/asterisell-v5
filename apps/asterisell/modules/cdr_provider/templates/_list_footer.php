<?php
use_helper('Markdown', 'OnlineManual');

echo insertHelp('

## CDR Providers

Call Detail Records (CDRs) can be retrieved from different sources/providers.

Each CSV file, containing CDRs, is associated to its CDR provider.

CDR providers must be first defined in this table, otherwise the application will generate an error, refusing to import the CSV file.

## Differences between CDR Providers and Vendors

A CDR provider is some computer related medium (a directory, a database table, some message queue, an FTP account, etc..) containing CDRs to import into Asterisell. It is the name of a computer communication channel, used for retrieving CDRs.

A Vendor is a fiscal entity that routes CDRs, and that can requires payments for this service. Every CDR routed by a Vendor, has a calculated cost, that can be also 0 (free).

In many configurations there is a one to one match between a Vendor and CDR provider. For example a Vendor X can put CDRs on an FTP account. But there can be configurations where the same CDR provider is used from many Vendors, or a Vendor uses more than one CDR provider.

');

