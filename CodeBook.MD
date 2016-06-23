---
title: "Getting and Cleaning Data Codebook"
---

# Identifiers
<li><code>subject</code> - Subjects who performed a set of activities</li>
</br>
<b>Range:</b> 1 - 30

</br>
<li><code>activity</code> - Set of activities carried out by each subject</li>
</br>
<b>Labels - Levels:</b></br>
WALKING - 1</br>
WALKING_UPSTAIRS - 2</br>
WALKING DOWNSTAIRS - 3</br>
SITTING - 4</br>
STANDING - 5</br>
LAYING - 6</br>

# Measurements
<b>Values:</b> Average for each <code>activity</code> and each <code>subject</code></br>
<b>Variables:</b> 79</br>

<li><code>time-BodyAccel-XYZ</code> Mean & Std</li>
<li><code>time-GravityAccel-XYZ</code> Mean & Std</li>
<li><code>time-BodyAccelJerk-XYZ</code> Mean & Std</li>
<li><code>time-BodyGyro-XYZ</code> Mean & Std</li>
<li><code>time-BodyGyroJerk-XYZ</code> Mean & Std</li>
<li><code>time-BodyAccelMagnitude</code> Mean & Std</li>
<li><code>time-GravityAccelMagnitude</code> Mean & Std</li>
<li><code>time-BodyAccelJerkMagnitude</code> Mean & Std</li>
<li><code>time-BodyGyroMagnitude</code> Mean & Std</li>
<li><code>time-BodyGyroJerkMagnitude</code> Mean & Std</li>
<li><code>freq-BodyAccel-XYZ</code> Mean & Std & MeanFreq</li>
<li><code>freq-BodyAccelJerk-XYZ</code> Mean & Std & MeanFreq</li>
<li><code>freq-BodyGyro-XYZ</code> Mean & Std & MeanFreq</li>
<li><code>freq-BodyAccelMagnitude</code> Mean & Std & MeanFreq</li>
<li><code>freq-BodyAccelJerkMagnitude</code> Mean & Std & MeanFreq</li>
<li><code>freq-BodyGyroMagnitude</code> Mean & Std & MeanFreq</li>
<li><code>freq-BodyGyroJerkMagnitude</code> Mean & Std & MeanFreq</li>

## Measured Variable Names
Variable names are made descriptive based on the following convention:</br>
<code>domain</code>-
<code>component</code>
<code>measurement device</code>
<code>jerk</code>
<code>magnitude</code>-
<code>extracted variable</code>-
<code>axis direction</code>

Examples:</br>
<code>time-BodyAccel-Mean-X</code></br>
<code>time-BodyGyro-Mean-Y</code></br>
<code>time-BodyGyroJerk-Std-Z</code></br>
<code>time-BodyAccelJerkMagnitude-Mean</code></br>
<code>freq-BodyGyro-MeanFreq-Y</code></br>

### Extracted Measured Variables for Analysis
Variables extracted from the [source](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI HAR Dataset.zip) data set are:
<li><code>-Mean</code> - from <b>mean()</b>, the mean value</li>
<li><code>-Std</code> - from <b>std()</b>, the standard deviation</li>
<li><code>-MeanFreq</code> - from <b>meanFreq()</b>, the weighted average of the frequency components to obtain a mean frequency</b>

### Other Measured Variable Name Components
<li><code>time-</code> - time domain</li>
<li><code>freq-</code> - frequency domain</li>
<li><code>Body</code> - body component</li>
<li><code>Gravity</code> - gravity component</li>
<li><code>Accel</code> - accelerometer measurement</li>
<li><code>Gyro</code> - gyroscope measurement</li>
<li><code>Jerk</code> - jerk signals</li>
<li><code>Magnitude</code> - magnitude of the 3-dimensional signals</li>
<li><code>-XYZ</code> - 3-axial signals in the X, Y, and Z directions</li>
</br>