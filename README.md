# schwinnTcxMeld
This simple R script will combine Schwinn Spinning data with Garmin data.  Do you use Schwinn spinning bikes at your gym, but you use a Garmin watch that won't sync data with the Schwinn head unit?  Then this script is for you (if you use R).  Usage is very simple, see example below.

The script leverages the fact that both data sources (TCX file and Schwinn file) will have your heartrate information.  Based on this, we can stitch the two files together and take the useful data from them both.

# Assumptions
1. You use a chest heart rate monitor (HRM) that will connect to the Schwinn headunit and your watch at the same time.
1. You use a Schwinn bike with an Echelon head unit (red ones, not black).
2. You use `R` and have it properly installed.
2. You have data.table, lubridate and zoo installed.
3. You have not used laps in your Garmin activity.  (This may be added in the future, but for now, the script won't deal with it correctly.)
4. You don't care about losing the temperature information (if your watch supports that).
5. You don't care that Schwinn bikes are horribly calibrated and provide almost worthless data beyond RPM.  Keep riding the same bike for useful comparison of progress over time.

# Process flow
1. Start Garmin Forerunner Spin Activity, ensure HRM is connected
1. Pedal Schwinn and turn on head unit, make sure the HRM is connected
1. Insert USB stick to Schwinn Echelon unit
1. SPIN, baby, SPIN!  Use multiple laps on the Schwinn unit, if you want, but only a single lap on the Garmin watch.
1. Hit end on Schwinn Echelon unit
1. Stop Garmin activity
1. Pull out USB stick
1. Save activity and sync with Garmin Connect (GC)
1. Download activity TCX file from GC
1. Attach USB stick to your computer
1. Run below test code, substituting in your file locations
1. Upload new TCX file to GC
1. Delete old GC activity
1. Bask in the data richness you now have

# Possible Upgrades
If I get motivated, I will take (and post) the GC data in the 'fit' format, so that things like Temperature will not be lost.