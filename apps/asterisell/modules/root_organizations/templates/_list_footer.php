<?php
use_helper('Markdown');
// Default culture is en_US
//
echo insertHelp('

## Organizations

Organizations are a hierarchies of departments, users, extensions, and so on.

## Users and Permissions

Every part of an organization can have many users with different roles. Users can view calls, receive reports, and so on, according their roles.

## Billing

The root organization is always billed (it will receive generated invoices).

An organization can be composed of children organizations, that can be billed separately, if the corresponding party is of type "billable".

## Changes of Organization Hierarchy

Organizations can change their hierarchy structure with time. The structure of the past is not lost, and calls are always rated according the organization hiearchy of the date of the call.

On the contrary users will have always the current role in the organization hierarchy, and their role in the past is not take in account. This for obviously security reasons.

');

?>