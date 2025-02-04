# Revision history for MiniCI

## 0.1.4 -- 2025-02-04

* Fix invocation of `minici run` without arguments
* Fix that empty temporary dir was not deleted in some cases
* Add explicit `--since-upstream` option for the `run` command

## 0.1.3 -- 2025-01-25

* Run jobs based on configuration in associated commit
* Configurable number of concurrently running jobs (using `-j` option)
* Concurrently run jobs for multiple commits
* Properly cancel and clean up jobs on user interrupt
* Added `--new-commits-on` and `--new-tags` options for `run` command to dynamically generate jobs based on branch/tags changes
* Support for GHC up to 9.12

## 0.1.2 -- 2024-07-30

* Explicit run command
* Support for GHC up to 9.10

## 0.1.1 -- 2023-04-25

* Support for GHC 9.2
* Added `-V` command-line switch to show version.

## 0.1.0 -- 2023-02-04

* First version.
