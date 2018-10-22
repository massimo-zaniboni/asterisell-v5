Features
========

Rate Calls
----------

Specify rating plans using a powerful domain specific language.

Group customers or extensions into distinct price list categories.

Change rates, and price lists assignments over time.

Support for bundle rates, and number portability.

::

  # Example of a rate plan
  rate {
    id: outgoing
    match-call-direction: outgoing
  
    rate {
      id: wholesale
      match-price-category: wholesale
      # this part of rate plan is applicable
      # only to customers in "wholesale" category.
  
      external-rate {
        # this is an external CSV file,
        # where each line has a telephone prefix
        # and a related cost.
        id: wholesale
        use: sell-wholesale
        set-cost-for-minute: this
      }
    }
  }

Billing
-------

Schedule the generation of invoices and reports.

Send invoices by email.

|screen_005|

Online Call Reporting
---------------------

Customers can view their calls online, using a simplified user interface.

Customers can export theirs calls to CSV files.

|screen_004|

Telephone Services
------------------

Sell telephone services to customers.

Change price list and service assignments over time.

|screen_001|

Inspect Rating Problems
-----------------------


Detailed errors reporting, with severity, problem description, effect and proposed solution.

Stats about unrated calls.
       
|screen_006|

Enable Resellers
----------------

Resellers sell VoIP calls to their customers, but using your VoIP infrastructure behind the scene.

|provider_reseller|

Install on Private Servers
--------------------------

Install Asterisell on your private servers.

You can customize nearly any aspect of the application, because you will receive application source code, and CDRs processing is based on customizable jobs.

::

  always_scheduled_jobs:
    - ImportDataFiles
    - SignalRatesToExportToResellers
    - ManageRateEvent
    - GarbageCollectBundleState
    - CheckCallCostLimit
    - CompareProviderCostWithCalculatedCost
    - GenerateScheduledReports
    - ReportsNotificationWorkflow
    - AdviseAdminOfNewProblems
    - BackupConfigurations
    - BackupSourceCDRS
    - BackupCDRS
    - BackupReports
  
  configure_jobs:
    - ConfigureDefaultParamsAndSettings
    - ConfigureHolidays
    - ConfigureDefaultResponsibleForEachErrorDomainType
    - InitWithDefaultMySQLStoredProcedures
    - InitTelephonePrefixes
    - LoadWorldTelephonePrefixesFromCSVFile
    - ConfigureCommunicationChannels
    - InitDefaultReportsForVoIPReseller
    - ForceReratingAtCurrentCronJobProcessor

Organizations Hierarchies
-------------------------

Configure customers with arbitrary complex organization hierarchies: offices, departments, and so on.

Support customers having multiple billable center costs.

Organization structure can change over time.

You can use Asterisell for call reporting inside a big organization.

Configure users with distinct permissions inside the same organization, and send them scheduled reports.
                    
|screen_002|

Import CDRs
-----------

Import Call Detail Records (CDRs) from local or remote database tables and files.

Many formats are supported, and more can be added.

Manage recognition of ported telephone numbers.

Merge CDRs from two or more collaborating VoIP servers, creating a single logical call.
                    
::

  # Call Flow Merge Rule
  name: outgoing_SBC
  direction: outgoing
  description: Outgoing call.
  merge: mgw1:MGW -> ipbx1:IPBX -> sbc1:SBC
  conditions:
    - mgw1.last_half_of_unique_id = sbc1.callid
    - mgw1.first_half_of_unique_id = ipbx1.last_half_of_unique_id
    - sbc1.status.calldestregid_isExternalVoIPVendor
  vendor: sbc1.calldestregid
  channel: implicit
  billsec_from: sbc1
  internal: ipbx1.src
  external: sbc1.dst

.. |image_monitor| image:: /images/header_screen_shoot_white_small.png
                   :width: 50%
.. |screen_001| image:: /images/screen_001.png
                :width: 50%
.. |screen_002| image:: /images/screen_002.png
                :width: 50%

.. |screen_003| image:: /images/screen_003.png
                :width: 50%

.. |screen_004| image:: /images/screen_004.png
                :width: 50%

.. |screen_005| image:: /images/screen_005.png
                :width: 50%

.. |screen_006| image:: /images/screen_006.png
                :width: 50%

.. |provider_reseller| image:: /images/provider_reseller.png
                       :width: 50%

Other features
--------------

- fast rating engine:

  - 8500 CDRS/s on dedicated fast host: 4 cores, 8GB RAM, 2xSSD
  - 4300 CDRS/s on shared cloud host: 1 core, 2GB RAM, SSD
  - 2600 CDRS/s on host with slow HDD 

- it uses TokuDB engine:

  - CDRS are saved in compressed state
  - SSD/HDD friendly because it performs a lot of sequential writes
  - performances do not degrade in case of a lot of data, but they remain constant, because all nodes of the btree are written in full state, not only leafs

- it pre-calculates daily grouped totals for CDRS, in order to speedup the Web user-interface, and common filters on data
- it can manage millions of monthly CDRS
- it detects problems in CDRS and rating plans, and it has detailed error messages 
- used in production for many years

Why not using Asterisell
------------------------

- the Web UI is dated
- it supports (up to date) only post-paid invoices
- during processing its store customer data into RAM, so it can support few thousands of customers, but not 10000 or more customers
