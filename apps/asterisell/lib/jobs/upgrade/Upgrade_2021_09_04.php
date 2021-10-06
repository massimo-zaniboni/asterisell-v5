<?php

class Upgrade_2021_09_04 extends AdminJobProcessor
{

    public function isCDRTableModified()
    {
        return true;
    }

    public function isDBUpgradeJob()
    {
        return true;
    }

    public function process()
    {
        $conn = Propel::getConnection();

        // The bundle-rates on "390" and "393" are not applied to "39[1245678]" and so the customer must know that they are "special" telephone numbers
        // rated using different criteria.        
        $conn->exec("update ar_telephone_prefix set geographic_location = 'Italy', operator_type = 'Special' where ar_telephone_prefix.prefix REGEXP '^39[12456789]' and (not operator_type like 'Solidal');");
        $conn->exec("replace into ar_telephone_prefix set prefix = '39445', operator_type = 'Solidal', geographic_location = 'Italy';");
                
        return '';
    }
}
