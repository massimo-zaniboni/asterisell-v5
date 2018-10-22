# Fixing errors in source CDRS

Source CDRS are imported and saved in ``ar_source_cdr`` table, and every next rating commands will read them, and rerate them.

Sometime there can be a VoIP server that generated bad data, and so CDRS of the past had to be deleted, or fixed.

The utility `php asterisell.php data export-cdrs` accessible after
executing `fab connect:INSTANCE` can export source CDRs of a time-frame.
They can be changed. There are some utilities on directory
`admin/script/source\_cdrs\_fix`, and other can be created.

The transformed files, can be put into instance directory
`admin/data_files/messages/input` and they will be imported again in
Asterisell. They are status files, so the previos version of source CDRs
on the same time-frame will be deleted, and replaced with the new
version.

A copy of the original exported source-cdrs can be maintained for a
while, in order to restore them in case of errors.

