# Change of rates

[csv-rate] and [main-rate-plan] can change over time. Asterisell will remember all past versions, and apply the correct one in case of rerating of CDRS of the past.

Rates are modified adding a new rate plan, with an initial
validity date. All the calls before this date are rated using the old
version of the rating plan, while calls from the specified date, are
rated using the new version.

## Changes of CSV rates

CSV rates can freely changes also within the same billing time-frame, and also more than one time. The CDR will be rated using the most recent (and active) [csv-rate] according the call-date of the CDR.

## Changes of main rate plans

A [main-rate-plan] can change only at the beginning of a new billing time-frame. This is not a big constraint, because one can update a current [main-rate-plan] taking in considerations more price-categories and other parameters, and CDRS will be rerated according it, and then activating the definitive [main-rate-plan] at the end of the rating time-frame.

## Fixing of rate plans

A [main-rate-plan] or a [csv-rate] can be updated without adding a new version, but in-place. All unbilled CDRS will be automatically rerated using the new settings.
