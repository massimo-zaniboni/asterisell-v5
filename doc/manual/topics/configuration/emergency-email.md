# Emergency email

Errors can be also signaled by email, and sent to all administrators with one or more of these roles:

  - `Notified for warning errors`
  - `Notified for errors`
  - `Notified for critical errors`

Critical errors are fired in case of:

  - high call costs surpassing the limit of an account of more than two
    time the configured limit (otherwise it is considered a warning)
  - there are too much calls not rated
  - there is a critical erorr, avoiding the processing of CDRs
  - and so on

Asterisell tries to reduce the number of emails notifications, signaling
you only when a certain type of error doubled is appareance respect the
past signaled email. In any case there can be false allarms, and some
noisy.

You can define another administrator account, with a distinct email, and
with only this role `Notified for critical errors`. In this way you can
be sure that the email is sent only in case of very important
problems, and it acts like an emergency email. It can be linked to a
pager.



