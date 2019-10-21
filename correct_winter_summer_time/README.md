# Correcting summer and winter time differences

In movement data, it is common that animals are monitored for a long time and in regions that
have periods daylight saving time (something like summer and winter times, light saving purposes).
This may create some errors in the way the data is stored, either in the way collars store data using
local time or in the way R converts the data to POSIXct objects and using the computer
local time as a reference.

This script is intended to help soving this issues, yet the data example we had we was not good, so it is 
still unfinished. (but the essence of the code is there!)
