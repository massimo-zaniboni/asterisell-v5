# Debug rating process

## Asterisell error messages

Open the error message, because it is formatted in a clear
way, respect the inline error.

It is possible inspecting the applied rate to a CDR, in the online call
report, clicking on the cost or income.

## Rerating in debug mode

It is possible generating more debug info, about applied and unapplied
rates, rating in debug mode. In the Asterisell instance admin directory:

```
    fab help
    
    fab cron_disable:INSTANCE
    # because every time CDRs are rerated
    # the debug info will be lost.
    
    fab connect:INSTANCE
    
    php asterisell.php debug rerate YYYY-MM-DD
    php asterisell.php debug jobs
    
    # debug and change rates, and then
    php asterisell.php debug rerate YYYY-MM-DD
    php asterisell.php debug jobs
    
    exit
    fab cron_enable:INSTANCE
```

