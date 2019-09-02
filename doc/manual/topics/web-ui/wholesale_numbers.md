# Wholesale numbers

## Usage scenario

  * originator C of an international call
  * wholesale provider P routes to local/national provider P
  * provider P acts like a proxy and route the call using cheaper national/local providers
  * the wholesale income is what customer C pays for the call to provider P
  * te wholesale cost is what provider P pays to reseller R for routing the call

## Wholesale carriers

The telephone number owned by the reseller R can be owned by a national telephone carrier/operator.

This information is not mandatory, so you can create a virtual/dummy carrier in case.

## Asterisell usage workflow

The majority of data is inserted using CSV files, because usually there are a lot of wholesale numbers and a WEB UI can be too much slow.

  * Provider P creates an initial batch of unassigned (FREE) or assigned to a reseller wholesale telephone numbers using a CSV file.
  * Unassigned (FREE) telephone numbers can be later assigned to resellers using a CSV file.
  * Assigned numbers can be moved between resellers using a CSV file.
  * An history of changes is maintained.

Every CSV file contains only the differences respect the past. Unaffected telephone numbers will be not listed in a CSV file.
CSV files can be relatively small because they will contain only new data.

A CSV file can be downloaded, fixed and uploaded again. Doing so history can be fixed/overwritten.

Usually these CSV files can be not created from scratch because Asterisell can return pre-filled version of them with relevant data as the unassigned telephone numbers, or the numbers of a certain reseller and so on.

## Physical limitations

Because every wholesale number is kept in RAM during rating not more than 100,000 (100K) wholesale numbers should be managed.

For reducing load one can assign range of wholesale numbers with the format "123XXX" (i.e. from "123000" to "123999") or "123*".
In this case only one entry will be used.

Wholesale numbers are all alias of the reseller. So the provider P can not filter/group calls by single wholesale number, but only for the entire reseller.

On the reseller side, numbers can be managed in a distinct way because they are stored as normal extensions.

## Wholesale CSV files

The two most important fields are:

  * "Telephone number": the wholesale telephone number affected by the changes.
  * "From timestamp": when the changes will be applied. Before this date the old settings of the number will be used.

As already said a CSV file contains only telephone numbers having changes respect the past.

This is an example of CSV file:

```
Effect,From timestamp,Last date comment,Action comment,Reseller,Carrier,Telephone number,Cost,Income,Extension codes
ALL,2017-06-01 00:00:00,,CREATE-FREE,FREE,TELCOM-IT,3912345611,0.1,0.15,
ALL,,,CREATE-FREE,FREE,TELCOM-IT,3912345623,0.1,0.15,
ALL,,,CREATE-FREE,FREE,TELCOM-IT,3912345635,0.1,0.15,
ALL,,,CREATE-FREE,FREE,TELCOM-IT,3912345647,0.1,0.15,
ALL,,,ASSIGN,CHP,TELCOM-IT,3912345615,0.1,0.15,
ALL,,,ASSIGN,CHP,TELCOM-IT,3912345627,0.1,0.15,
ALL,2017-05-01 00:00:00,,CREATE-FREE,FREE,EUTELEPHONE,3912345614,0.1,0.15,
ALL,,,CREATE-FREE,FREE,EUTELEPHONE,3912345626,0.1,0.15,
ALL,,,CREATE-FREE,FREE,EUTELEPHONE,3912345638,0.1,0.15,
ALL,,,ASSIGN,CHP,EUTELEPHONE,3912345654,0.1,0.15,
```

  * "From timestamp", in "YYYY-MM-DD hh:mm:ss" format.
  * if "From timestamp" is left empty then the previous specified value will be used. In this way one can specify only one value and implicitely copy and paste it to all other rows.
  * two or more "From timestamp" values can be used in the same CSV file.
  * "Last date comment" is a field completed from Asterisell, having no effect for imported CSV files. It is the date of the previous modification action on the same telephone number.
  * "Effect" field can be "ALL" or "RESELLER".
  * with "Effect" set to "ALL", all the previous assignations in the same "From timestamp" will be replaced with the content of this new CSV file.
  * with "Effect" set to "RESELLER", all the previous assignation in the same "From timestamp" but only for the same "Reseller" will be replaced with the content of CSV file.
  * "RESELLER" setting is useful for specifying changes to apply only to a specific reseller, without affecting changes of other resellers. It is usually generated from Asterisell for proposing free numbers to assign to a reseller.
  * with "Effect" set to "COMMENT" the line will be a simple comment and it is ignored.
  * if "Effect" is empty, the previous "Effect" field will be considered, so you can complete only the first row
  * "Action comment" is a field completed from Asterisell, having no effect for imported CSV files. It documents the actions on the telephone number (e.g. CREATE, ASSIGN, MOVE)
  * if the field "Reseller" is empty then the wholesale number is considered unassigned (FREE), otherwise it must contain the code of a valid reseller.
  * if the field "Reseller" contains the special identifier "DELETE" then the telephone number is considered not any more owned from the provider P, i.e. it is not unassigned and it does not exist anymore.
  * "Cost" and "Income" must be specified in standard international decimal notation, e.g. "10.23".
  * "Extension codes" are the internal CDR extension codes that Asterisell had to use for recognizing the calls associated to the specified "Telephone number". It can be empty: in this case a default value will be set according the value of "Telephone number" and the customizations in ``asterisell_instances.py``.

