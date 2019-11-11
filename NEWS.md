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
