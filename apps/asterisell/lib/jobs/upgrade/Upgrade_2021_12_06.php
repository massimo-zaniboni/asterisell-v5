<?php

/**
 * Migrate from "apache" to "nginx" user.
 */
class Upgrade_2021_12_06 extends AdminJobProcessor {

    public function isCDRTableModified() {
        return true;
    }

    public function isDBUpgradeJob() {
        return true;
    }

    public function change_ownership($directory, $from, $to) {
        shell_exec("find $directory -user $from -exec chown $to {} \\;");
        shell_exec("find $directory -group $from -exec chown :$to {} \\;");
        
        return "Changed ownership of apache files in directory $directory";
    }

    public function quick_upgrade() {
        $r = "";
 
        $r .= "\n" . $this->change_ownership("/var/lib/", "apache", "nginx");   
        $r .= "\n" . $this->change_ownership("/etc/", "apache", "nginx");
 
        $r .= "\n" . shell_exec("systemctl restart php-fpm.service");
        $r .= "\n" . shell_exec("systemctl start nginx.service");
        return $r;
    }
    
    public function process() {
        $r = "";
 
        $r .= "\n" . shell_exec("systemctl stop nginx.service");
        $r .= "\n" . shell_exec("systemctl stop php-fpm.service");
   
        $r .= "\n" . $this->change_ownership("/var/", "apache", "nginx");   
        $r .= "\n" . $this->change_ownership("/etc/", "apache", "nginx");   
 
        $r .= "\n" . shell_exec("systemctl start php-fpm.service");
        $r .= "\n" . shell_exec("systemctl start nginx.service");

        return $r;
   }
}
