# Getting and Cleaning Data README

## <a name="intro"></a>Introduction
The purpose  of this project is to demonstrate an ability to collect, work with, and clean a data set.</br>
The [source](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI HAR Dataset.zip) data set contains a set of files, a subset of which will be used in this analysis (see [<b>Files</b>](#files) section below).</br>
The [<b>Analysis Script</b>](#script) section describes how the data is acquired and cleaned.</br>
Finally, the steps for generating a tidy data set is outlined in the [<b>Creating the Tidy Data File</b>](#tidy) section.

## <a name="files"></a>Files
<b>Common Files</b>
<li><code>features.txt</code> - List of all features</li>
<li><code>activity_labels.txt</code> - Links the class labels with their activity name</li>
</br>
<b>Test Files</b>
<li><code>test/X_test.txt</code> - Test set</li>
<li><code>test/y_test.txt</code> - Test labels</li>
<li><code>test/subject_test.txt</code> - Test subjects who performed the activity for each window sample</li>
</br>
<b>Train Files</b>
<li><code>train/X_train.txt</code> - Train set</li>
<li><code>train/y_train.txt</code> - Train labels</li>
<li><code>train/subject_train.txt</code> - Train subjects who performed the activity for each window sample</li>
</br>
[[back to top]](#intro)

## <a name="script"></a>Analysis Script

The <b>run_analysis.R</b> script carries out the following:

1. Sets working directory as defined by the user
</br>&nbsp; a. If undefined, default is set to the current directory the script is run in
2. Checks if all the files as listed [above](#files) are available in the directory
</br>&nbsp; a. If file(s) are not available, checks if <b>UCI HAR Dataset.zip</b> file exists
</br>&nbsp; b. If file does not exist, downloads from [source](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI HAR Dataset.zip)
</br>&nbsp; c. Unzips <b>UCI HAR Dataset.zip</b>
</br>&nbsp; <i>Note: Ensure file is renamed <b>UCI HAR Dataset.zip</b> if manually downloaded prior to running the script.</i>
3. If all files are available, begins extracting and renaming features for <b>mean()</b>, <b>std()</b>, and <b>meanFreq()</b>
4. Creates TEST and TRAIN data tables with <code>subject</code>, <code>activity</code>, and <code>features</code> extracted above
5. Merges TEST and TRAIN data tables
6. Sets descriptive <code>activity</code> names
7. Renames columns with meaningful names
8. Averages each variable for each <code>activity</code> and <code>subject</code>
9. Exports data to <b>tidydata.txt</b>

</br>
[[back to top]](#intro)

## <a name="tidy"></a>Creating the Tidy Data File
1. Source file <code>run_analysis.R</code>
2. Call function <code>run_analysis(x)</code>, where <code>x</code> is the working directory
3. Tidy data file <b>tidydata.txt</b> is created in the defined working directory

</br>
[[back to top]](#intro)
</br>