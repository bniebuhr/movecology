# Fixing GPS data and preparing it to movement analyses

By Bernardo Niebuhr (bernardo_brandaum@yahoo.com.br) and [Karl Mokross](https://sites.google.com/view/karlmokrossresearch/home)

The [main script](https://github.com/bniebuhr/movecology/blob/master/fix_GPS_data/analysis_BLT_yness.R) is a code to fix and organize raw GPS data. It includes:
- Removing large empty gaps (in this example, 30 min.)
- Regularizing trajectories
- Filling small gaps (by interpolation)
- Applying a Kalman Filter (Smoothing trajectories)
- Applying a GAL Filter (Correcting extreme wrong moves)
- Obtaining movement parameters
- Extracting distance from forest edges from raster maps

This code was originally used to analyze Black Lion Tamarin (*Leontopithecus chrysopygus*) GPS data.

You may also find another [script](https://github.com/bniebuhr/movecology/blob/master/fix_GPS_data/calculate_fix_rate_success.R) to calculate the fix rate success for GPS collars by comparing the expected and actual number of GPS locations recorded by GPS devices.

If you have any questions, feel free to contact us.
