# CDRs provider

A CDR provider is some computer related medium (a directory, a database
table, some message queue, an FTP account, etc..) containing CDRs to
import into Asterisell.

Note that a [cdrs-provider] is not a [vendor], also if in many configurations
there is a one to one to one match between a Vendor and a CDR provider. There
can be configurations where the same CDR provider is used from many
Vendors (you retrieve CDRs from ony of your VoIP server, configured for
collaborating with many VoIP providers), or a Vendor can use more than
one CDR provider (the Vendor uses multiple FTP accounts).

