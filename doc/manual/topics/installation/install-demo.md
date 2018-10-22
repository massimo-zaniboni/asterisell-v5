# Install the demo instance


```
./fab.sh
# for entering in the instance management tool

fab help
# for showing commands and available instances

fab install:asterisell/demo
# for installing on "asterisell" host
# the "demo" instance
```

**warning**: The first `installation will be very slow because it will load
an entire Haskell compilation environment, with all modules and libraries.
This environment will be reused for all other instances, also on different
hosts.

If you change setting of the host or domain you can upgrade using

```
fab upgrade_conf:asterisell/demo
```
