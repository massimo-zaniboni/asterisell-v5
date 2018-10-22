# Web UI configurations on provider

## Reseller code

Decide an internal name/code for the reseller, and complete ``Entities -> Resellers``.

## Reseller as a billed customer

The provider will sell the calls to a single customer, representing the reseller. 

In ``Entities -> Customers`` create a customer associated to the reseller:

  - the party will have the billing information of the reseller
  - the reseller field will be set to the specific reseller, instead of NULL
  - the VoIP extensions will identify the CDRS to export to the reseller

## Reselled extensions

For each [extension] set the ``export-code`` field. It will be the [extension] code as seen from the reseller. In this way it is possible masking the extension code used on the provider side. 

It is possible to execute this action also in batch mode, executing 

```
./fab.sh

fab connect:HOST/INSTANCE
php asterisell.php data complete-reseller-export-code
```

## Service CDRS

TODO complete
