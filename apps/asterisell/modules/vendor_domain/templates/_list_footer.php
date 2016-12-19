<?php
use_helper('Markdown');
// Default culture is en_US
//
echo insertHelp('

## VoIP Call Channel

Calls can be rated according the vendor managing the call, and optionally the communication channel used for routing them.

The import procedure of CDRs assign a channel to every CDR. The channel is used for associating the vendor and the communication channel to the call, according the data in this table.

## Configuration

Usually the best method for configuring this table, is not inserting any value, then start CDR rating, and see if Asterisell complains for some missing channel. Then you can compile this table accordingly.

Channel names to use, depends from the content of CDRs generated from the provider, and from the Asterisell code processing them. So you can obtain details about their configuration from your VoIP provider documentation, and from Astiresell assistance.

');

?>