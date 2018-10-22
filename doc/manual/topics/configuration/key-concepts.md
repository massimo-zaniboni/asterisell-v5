# Key concepts 

Asterisell configuration is the most difficult part of Asterisell usage, and in some case it requires writing ad-hoc integration code,
for importing new CDRs formats, or new type of rates. In case contact the assistance.

Asterisell configuration is an iterative process:

  - initial configuration attempt
  - wait CDRs are rerated, using the new configurations
  - study error messages, and improve configuration accordingly
  - repeat until there are no more left signaled errors

Asterisell rates CDRs in a conservative way:

  - rate a CDR only if it is sure it is all correct
  - in doubt refuse to rate the CDR and signal the problem

Asterisell error messages are informative, because they contains:

  - the type of error
  - the effect/consequences of the problem
  - the suggested solution
  - the number of CDRS that are affected by the problem
  
After initial configuration, the application works in a rather automatic way, signaling only new rating problems, and maintenance is easier.

