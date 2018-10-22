# Key concepts

Asterisell uses PHP for the Web user interface, and for coordination of jobs, and Haskell for rating the calls.

See the [system overview](../installation/system-overview.md).

TokuDB DBMS engine is a good fit for VoIP processing because:

  - it can compress data with common prefixes
  - it can compress btree leaf nodes using snappy algorithm
  - it does not worn-out SSD disks during writes, because it performs a lot of sequential writes
  - it is reasonably fast also on traditional HDD disks, for the same reason
  - its performances do not degrade in case of a lot of data, like many traditional btree implementations, but they remain constant

The rating engine:

  - is coded in Haskell
  - import CDRS from multiple sources, and store them in ``ar_source_cdr`` table
  - rates CDRS according a rating-plan written in a powerful domain-specific language
  - send rated CDRS to ``ar_cdr`` table, mantaining ``ar_source_cdr`` in their original form
  - pre-calculate daily grouped totals for CDRS, in order to speedup the Web user-interface

The rating engine has mainly a batch work-flow, and it process only terminated calls.
It is fast because on basic hardware it can rate  approximately 10K CDRS by second.

The [jobs-processor] execute all the Asterisell jobs. Asterisell can be easily customized adding more jobs to the sequence.
