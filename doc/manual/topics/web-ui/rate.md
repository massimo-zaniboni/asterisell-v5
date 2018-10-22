# Rate

There must be a mandatory [main-rate-plan] named:
  
  - ``main-cost-rate``
  - ``main-income-rate``

See ``Rate->Rate Formats`` Web form for a list of supported [csv-rate] rate formats.

## Change of rates

New rates can be a new version of an old rate:

  - they have the same name of the old rate
  - they have an activation date
  
The old version will be kept, and CDRS will be rated with the old rate if their calldate is before the activation of the new rate.
