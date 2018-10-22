# Call reporting mode

In Call reporting mode, Asterisell is not used for billing the calls, but only for monitoring the calls done 
(probably from some big organization, with some complex organization structure), and comparing respect 
invoiced costs.

This operation mode must be setted in the [instances-configuration-file]. 

The admin had to specify:

  - a cost [main-rate-plan] assign cost 0 to all calls
  - an income [main-rate-plan] assigning the expected cost to the calls
