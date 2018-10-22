# Access the demo instance

You can connect with 

```
./fab.sh
# for entering the management tool

fab connect:asterisell/demo
# for SSH login into "asterisell" host
```

The connection utility will show also the commands for HTTPS (web) access.

An URL like <https://www.example.net/admin> open the admin instance of
Asterisell:

  - only admins can access it, and not normal users
  - the PHP application here has full write/read access to the database

An URL like <https:/www.example.net/> open a normal instance of
Asterisell:

  - only normal users (your customers) can access it
  - the PHP application and database connection has limited read access
  - a safe/limited set of PHP modules are installed, for reducing security problems


