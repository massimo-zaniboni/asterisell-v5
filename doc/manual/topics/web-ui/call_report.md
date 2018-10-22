# Call report

## Recent calls

Recent calls are the calls that are not yet billed to customers, as specified in ``Params->Official Call Date``.

When you change some rating param, or some configuration, recent calls are rerated by default, in order to apply the last changes. In this way changes are applied only to recent calls, but not to already billed calls.

Usually you set the ``Official Call date`` only one time, then it is automatically advanced every time you confirm the billing reports. A billing report is a report having the flag "Billing document" enabled.

If you miss to update the official call date, then a lot of calls will be rated every time you change some param.

## Sorting

CDRs can be sorted on various columns, but the only guarantee fast sorting method is on calldate, because it can take advantage of fast indexes. Sorting on other columns will be a lot slow in case there are many CDRS.

## Debugging rate CDRS

The admin (and only the admin) can click on calls cost/income field, for inspecting the rating details. More details are present if CDRS are rated in debug mode.

## System calls

System calls are associated to internal services/costs. Usually they are used for managing system related services/costs, like services imported from [vendor].

The admin can view all the type of calls, while customers see only allowed direction, as specified in the [instances-configuration-file].

## Cost Precision

Costs are displayed using the default currency precision but they are stored (and summed) using full precision, as specified in the [instances-configuration-file].

The precision to use in calcs is specified in the param "currency_decimal_places_in_invoices".
