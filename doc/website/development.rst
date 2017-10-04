.. _Asterisell: https://www.asterisell.com

Asterisell Code Development
===========================

These notes are interesting only if you want to extend the Asterisell application source code.

.. contents:: Table of Contents
   :depth: 2
   :backlinks: top
   :local:

How installing the symfony Docker DEV container
-----------------------------------------------

Read notes inside `scripts/dockerfiles/symfony_dev`.

How changing the database schema
--------------------------------

Change the file ``config/schema.yml``.

Inside the Symfony DEV Docker Container execute

::
  cd scripts
  ./makedb.sh

The new SQL model is generated inside ``data/sql/lib.model.schema.sql``.

Test the application.

Add upgrade jobs extending the database also in already production instances. TODO complete this

How generating UI web modules
-----------------------------

Inside the Symfony Docker DEV Container in:

::

  php symfony propel:generate-admin asterisell ArReportScheduler --module=report_scheduling

: if you add a new class, update also init database scripts, and
deletion related.

*  generate module normally
*  in generator.yml change generator.class to sfPropelAdminGenerator
*  in actions.class.php:
   *  remove the initial require once of lib files
   *  consider using the option for sorting pagination also on ID

*  remove in lib of module the two configuration files
*  in apps/asterisell/config/routing.yml remove the initial code
*  in config, add security settings for admin access
*  add the module into the Asterisell Menu

From ``config/routing.yml `` remove code like:

::

    ar_role:
      class: sfPropelRouteCollection
      options:
        model:                ArRole
        module:               role
        prefix_path:          /role
        column:               id
        with_wildcard_routes: true

    ar_user:
      class: sfPropelRouteCollection
      options:
        model:                ArUser
        module:               user
        prefix_path:          /user
        column:               id
        with_wildcard_routes: true

    ar_extension_has_code:
      class: sfPropelRouteCollection
      options:
        model:                ArExtensionHasCode
        module:               extension_code
        prefix_path:          /extension_code
        column:               id
        with_wildcard_routes: true

    ar_organization_unit:
      class: sfPropelRouteCollection
      options:
        model:                ArOrganizationUnit
        module:               organization_unit
        prefix_path:          /organization_unit
        column:               id
        with_wildcard_routes: true

Use code like this:

::

    generator:
      class: sfPropelAdminGenerator
      param:
        model_class:           ArExtensionHasCode
        theme:                 admin
        non_verbose_templates: true
        with_show:             false
        singular:              ArExtensionHasCode
        plural:                ArExtensionHasCodes
        route_prefix:          ar_extension_has_code
        with_propel_route:     1
        actions_base_class:    sfActions

        fields:
          ar_extension_id: { name: Extension }
          ar_extension: { name: Extension }
          code: { name: Code, help: "An extension can have one or more code (telephone numbers) that are valid alias of them." }

        list:
          title: Extension Codes (alias telephone numbers associated to an Extension)

          display: [ar_extension, =code]
          filters: [ar_extension_id, code]
          sort: [code, asc]

        edit:
          title: Extension Codes (alias telephone numbers associated to an Extension)
          display: [ar_extension, code]

How profiling and solve speace leaks of the Haskell Rating Engine
-----------------------------------------------------------------

* Enable ``debug_mode`` in ``fabric_data/lib.py``.
* The first time execute a ``fab pedantic_upgrade:instance_name``, and then ``fab upgrade:instance_name``, for forcing a clean of the build files, and a recompilation with profiling options enabled.
  * Choose the profiling options to execute, according notes on `<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html>`_
* ``fab upgrade:instance_name`` for enabling them.
* ``fab connect:instance_name`` and then ``php asterisell.php debug rerate`` and ``php asterisell.php run jobs`` for executing a rating pass with the profiling enabled.
* Inspect the ".hp" and ".prof" produced files. Use as example ``rating_tools/utilities/process-haskell-profiling.sh``.
* At the end of the profiling process, disable ``debug_mode``, and execute a ``fab pedantic_upgrade:instance_name`` again.

It is possible executing only specific parts of the rating engine using the ``--run-level`` options. See the source code of the rating engine for more info.

Code gist
---------

Adding LOG message
~~~~~~~~~~~~~~~~~~

Inside an action

::

  $this->logMessage('help me!', 'info');

Outside an action

::

   sfContext::getInstance()->getLogger()->info($message);
   sfContext::getInstance()->getLogger()->err($message);

Inside a template if the error must be viewed on the web toolbar

::

   echo log_message(...);


Project administration
----------------------

How publishing a new release
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Make sure to use a recent version of Stackage LTS, consulting ``https://www.stackage.org/``
and update ``stack.yaml`` inside ``rating_tools/rate_engine``.
* Run regression tests.
* Connect to a demo instance and run ``php asterisell.php debug stress-rerating [MAX-DAYS-IN-THE-PAST] [TIMES]``.
* Update ``VERSION`` file.
* Update manual.
* Git commit.
* ``git tag -a vX.YY -m "Version X.YY"``
* Git push to GitHub repo.
* Git push to Gitea OSS repo.
* Git push to repo of customers.
* Rsync the website.
