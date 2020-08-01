Student Performance Data Set Downloaded from
https://archive.ics.uci.edu/ml/datasets/Student+Performance

Reference: P. Cortez and A. Silva. Using Data Mining to Predict
Secondary School Student Performance. In A. Brito and J. Teixeira
Eds., Proceedings of 5th FUture BUsiness TEChnology Conference
(FUBUTEC 2008) pp. 5-12, Porto, Portugal, April, 2008, EUROSIS, ISBN
978-9077381-39-7.

- student.txt:          description of data
- student-mat.csv:      data for mathematics
- student-por.csv:      data for Portuguese language
- student-merge.R:      script for merging two data files
- student-preprocess.R: script for pre-processing data for ROS example

This data approach student achievement in secondary education of two
Portuguese schools. The data attributes include student grades,
demographic, social and school related features) and it was collected
by using school reports and questionnaires. Two datasets are provided
regarding the performance in two distinct subjects: Mathematics (mat)
and Portuguese language (por). In [Cortez and Silva, 2008], the two
datasets were modeled under binary/five-level classification and
regression tasks. Important note: the target attribute G3 has a strong
correlation with attributes G2 and G1. This occurs because G3 is the
final year grade (issued at the 3rd period), while G1 and G2
correspond to the 1st and 2nd period grades. It is more difficult to
predict G3 without G2 and G1, but such prediction is much more useful
(see paper source for more details).
