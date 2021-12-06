# Configure instances

Edit the file ``asterisell_instances.py``. In particular the host and domain where installing a first DEMO instance of Asterisell.

The DEMO instance settings are in Python class ``DemoInstance`` and in the referenced classes ``DemoHost``, ``DemoInstanceHttpDomain`` and ``DefaultDemoInstance``.
By default it is installed on localhost (127.0.0.1) and it is accessible from ``http://127.0.0.1/demo/admin``, for the administration part, 
and ``http://127.0.0.1/demo`` for the customer part. The demo DEV instance, if installed, is accessible by default at ``http://127.0.0.1/dev/admin``.
You can freely customize them with the settings of the server where you want to install Asterisell.

Settings can be improved later, so initially start with good-enough settings.

The remote host will be configured using the instructions inside ``fabric_data/lib.py`` file.
It follows a DevOp philosohpy, so the installation script assumes having full control of the server configurations,
and that you use the host only for Asterisell instances. 
In case you install additional services, you may need customizing the ``asterisell_instances.py`` file.
In particular NGINX, PHP and other settings can be overwritten from the administration script.

