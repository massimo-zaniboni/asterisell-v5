# Custom jobs

Custom jobs usually are PHP files inside ``customizations`` directory. See [customized-file]. 

Some notes about them can be put in ``customizations/CUSTOM.md`` file, so they will be visible also on the online help (only for admins).

Custom jobs are configured inside ``asterisell_instances.py``:

- ``custom_files`` send them to the instance
- ``import_cdrs_jobs``, or ``custom_initial_always_scheduled_jobs`` or ``custom_export_cdrs_jobs`` schedule them

The [instances-configuration-tool] produce an ``apps/asterisell/config/app.yml`` file with the configured jobs.
