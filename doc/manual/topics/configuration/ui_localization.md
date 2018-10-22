# User interface localization

Every Asterisell instance supports only one configurable culture. 

In [instances-configuration-file] set ``culture`` with the correct locale, and update the instance. 
The culture influence the language of the user interface, formatting of
dates, money values, and so no.

## Translating to another language

The web user interface seen by customers can be translated to different
languages, while the admin user interface will remain always in
English because there are too much terms to translate, and there is no
payoff in translating it. 

For supporting another language:

  - copy the file `scripts/installation/i18n/messages.it.xml` to
    something like
    `scripts/installation/i18n/messages.<your-language-code>.xml`
  - translate the strings to your language:
      - the strings starting with "\_\_" are used inside reports, emails
        and other parts of the user interface generated from code
      - the strings starting without "\_\_" are usend inside the
        customer online call report
      - you can ignore this detail, and convert all the strings
        preserving the "\_\_" if it is present
  - update the code inside
    `apps/asterisell/lib/helper/CustomLocaleConversionsHelper.php`
  - execute `fab upgrade_conf:INSTANCE` for activating the new
    translations
  - consider to send pull requests/patches to support with the new
    translations, so they can be included in the official Asterisell
    release
