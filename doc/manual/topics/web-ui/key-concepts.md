# Key concepts

## Hierarchical structures

Hierarchical structure definitions (tree-like) are preferred respect
flat definitions (list-like):

  - Customers are defined using hierarchical structures, for example
    "Acme Organization/Accounting Office/John Smith".
  - Rating rules are defined using hierarchical structures, for example
    "acme-vendor-telecom/outgoing/emergency-number".

The advantage of hierarchical structures are:

  - common parameters can be set in the parent node, and they are
    inherited in the children nodes, that can eventually override them;
  - complex structures can be named referring only to the proper parent
    node;

## History preservation

New information does not replace old information, overwriting it, but it
is added to the database, specifying when it became active. So a
complete history of the past is preserved. The advantages are:

  - changes of rates and services can be planned in the future;
  - CDRs of the past can be re-rated, applying the (correct) old rate
    specifications;
  - customers can change price-categories, or subscription of services,
    without loosing the history of the past;
  - services can change prices, but old prices can (optionally and if
    configured) still applied to old customers;

## Conservative rating

Asterisell uses a conservative approach about CDR rating. In case of
doubt it does not rate a CDR, but it signals the problem.

## Official calldate

The official calldate is the date of the last billed CDRs. All CDRs
before this date are not re-rated automatically, because they are
considerd as already billed to customers, and so immodifiable.
CDRs after this date are
re-rated automatically every time configuration params change.

