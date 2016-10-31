# Fixing GPS data and preparing it to movement analyses

By Karl Mokross (kmkross@gmail.com) and Bernardo Niebuhr (bernardo_brandaum@yahoo.com.br)

This is a code to fix and organize raw GPS data. It includes:
- Removing large empty gaps (in this example, 30 min.)
- Regularizing trajectories
- Filling small gaps (by interpolation)
- Applying a Kalman Filter (Smoothing trajectories)
- Applying a GAL Filter (Correcting extreme wrong moves)
- Obtaining movement parameters
- Extracting distance from forest edges from raster maps

This code was originally used to analyze Black Lion Tamarin (*Leontopithecus chrysopygus*) GPS data.

If you have any questions, feel free to contact us.
