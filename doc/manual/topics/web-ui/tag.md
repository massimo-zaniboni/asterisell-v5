# Tag

Tags are used for tagging customers, and managing different reporting workflows. 
Apart this, there is not a full integration of tags in other parts of the application. 

In case there are mandatory TAGS, you can add a job of type "apps/asterisell/lib/jobs/checks/CheckExclusiveTags.php", specifying the TAG that are mandatory, and advising if there are unclassified customers. This will prevent that customers will not inserted in some reports.

