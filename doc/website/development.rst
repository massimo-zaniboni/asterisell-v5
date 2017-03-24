.. _Asterisell: https://www.asterisell.com

Asterisell Code Development
===========================

These notes are interesting only if you want to extend the Asterisell application source code.

.. contents:: Table of Contents
   :depth: 2
   :backlinks: top
   :local:

How Installing the Symfony DEV Container
----------------------------------------

Read notes inside `scripts/dockerfiles/symfony_dev`.

How Changing the Database Schema
--------------------------------

Change the file ``config/schema.yml``.

Inside the Symfony DEV Container execute

::
  cd scripts
  ./makedb.sh

The new SQL model is generated inside ``data/sql/lib.model.schema.sql``.

Test the application.

Add upgrade jobs extending the database also in already production instances. TODO complete this

How Generating UI Web Modules
-----------------------------

Inside the Symfony DEV Container:

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

Code Gist
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


