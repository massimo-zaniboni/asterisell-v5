# Download an Haskell generated profiling file, and convert it to a visualizable format.

rm RateEngine.svg
scp -P 22000 root@localhost:/var/www/asterisell/admin/RateEngine.hp .
scp -P 22000 root@localhost:/var/www/asterisell/admin/RateEngine.prof .

hp2ps RateEngine.hp
# NOTE: used for producing RateEngine.aux file with the association between
# SCC code and position in source code.
# The produced .PS file is not so much readable.

hp2pretty RateEngine.hp
# NOTE: produce a readable .SVG file.

eom RateEngine.svg &


