<?php
use_helper('Markdown');
// Default culture is en_US
//
echo insertHelp('

## Type of Channels

A VoIP call can be routed using some communication channel type, that is something like "Local Network", "PSTN Channel" and so on.

The used channel can be displayed (or not), to users, according the settings of the Asterisell static configuration file.

Calls can be rated according the vendor, and the used channel.

## Simple Case

If you are using only one or more VoIP provider for routing the calls, you will use probably only the "VoIP provider" channel, because the calls are routed through different channels from the VoIP provider.

You must define and use more channels, only if the VoIP provider apply a different rate, for every different channel used for routing the call. But usually this is not the case.

## Complex Case

If you are using different method for routing the calls, and every channel has a different cost/income, you must and define different channels.

');

?>