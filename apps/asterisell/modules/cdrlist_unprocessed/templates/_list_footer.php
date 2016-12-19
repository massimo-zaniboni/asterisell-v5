<?php
use_helper('Markdown', 'OnlineManual');
// Default culture is en_US
//
echo insertHelp('

## Unrated CDRs

In this list there are CDRs, with rating errors.

CDRs are linked to the error detail description. If the link is missing, the table with details must be generated again, forcing a rerating of the time-frame of the CDRs with problems.

The error detail description is not the exact error generated from the CDR, but it can be a problem generated from a CDR with a similar problem, because discovered errors are grouped into few distinct cases.
');
