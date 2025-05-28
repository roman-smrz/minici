# Revision history for MiniCI

## 0.1.7 -- 2025-05-28

* Added `log` command to show job log
* Added `extract` command to extract artifacts
* Added `--terminal-output` and `--log-output` options to set output style
* Run jobs by specifying full job id or reference

## 0.1.6 -- 2025-03-30

* Added `jobid` command resolving job reference to canonical ID
* Fix copying of used artifacts to appropriate working directory

## 0.1.5 -- 2025-03-20

* Accept job file path on command line
* Added `checkout` command
* Reference and checkout other repositories from job file
* Accept names of jobs to run as command-line arguments

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
