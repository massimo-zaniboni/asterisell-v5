# Download an Haskell generated profiling file, and convert it to a visualizable format.

rm RateEngine.svg
scp -P 22000 root@localhost:/var/www/asterisell/admin/RateEngine.hp .
scp -P 22000 root@localhost:/var/www/asterisell/admin/RateEngine.prof .
~/.cabal/bin/hp2pretty RateEngine.hp
eom RateEngine.svg &


