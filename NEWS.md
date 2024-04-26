# usefun 0.5.1

- Add `pr.boot()` for adding bootstrap CIs to a PR curve

# usefun 0.5.0

- Add `pr.test()` to compare two PR curves

# usefun 0.4.9

- Fix CRAN notes

# usefun 0.4.8

- add function `dec_to_bin` for converting decimal numbers to binary
- add function `partial_permut` to get partial random permutations of a vector

# usefun 0.4.7

- Removed `grDevices` import since it's not used anymore

# usefun 0.4.6

- Add tests for the plot and save functions. Total coverage is now 100%. Yay!
- Removed obsolete function `plot_string_to_file`

# usefun 0.4.5

- add parameter `direction` to function `get_roc_stats` to indicate the direction/ranking of the prediction values with respect to the positive class labeling
- add most important functions to `README.md`

# usefun 0.4.4

- add function `get_roc_stats` for calculating ROC statistics + tests

# usefun 0.4.3

- Added function `binarize_to_thres` for converting a matrix to a binary one based on a given threshold
- Renamed function `get_average_over_unique_values` to `get_stats_for_unique_values` and changed it's returned object class to a data.frame

# usefun 0.4.2

- Added better documentation for the `pretty_print*` functions

# usefun 0.4.1

- Improved `get_percentage_of_matches` handling of NA and NaN values

# usefun 0.4.0

- Added function `ldf_arrange_by_rownames` to rearrange a list of data frames

# usefun 0.3.5

- refined test for `add_row_to_ternary_df`: it can be used also for 
'binary' data.frame objects (filling in only 1 and 0's)

# usefun 0.3.4                                                                  
                                                                                
- added new `add_row_to_ternary_df` function

# usefun 0.3.3                                                                  
                                                                                
- simplified `is_empty` function                                                
- renamed `normalize_vector` function to `normalize_to_range`                   
- added new `get_ternary_class_id` function

# usefun 0.3.0                                                                  
                                                                                
- Changed name of the package                                                   
- Common naming scheme for all functions: use *snake_case*

# usefun 0.2.0

- Finished writing all tests
- Completed documentation on all functions
- Added a `NEWS.md` file to track changes to the package
- First stable version
