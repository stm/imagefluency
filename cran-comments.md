## Resubmission
This is a resubmission. In this version I have:

* fixed `img_complexity()` normalization for array inputs outside the `[0, 1]`
  range
* updated the corresponding complexity test so it checks the normalization
  behavior directly rather than relying on one platform-specific compressed
  file-size ratio
* modernized the test suite by removing legacy `context()` calls
* added optional `collapse`-based performance improvements while keeping base R
  fallbacks available
* redesigned the local Shiny dashboard with multi-image upload, integrated
  results, image comparison, and CSV export
  

## Test environments
* local macOS Sonoma 14.8.1, R 4.4.0
* via gh-actions: macOS, windows, ubuntu (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.
