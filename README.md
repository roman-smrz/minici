MiniCI
======

MiniCI runs jobs defined in the `minici.yaml` file in the root of the project
(on the same level as the `.git` directory). With that, `minici` invocation can
execute the jobs for local commits that are not yet in upstream (remote) branch
or for any manually given commit range.


Job definition
--------------

The basic top-level elements of the YAML file are `job <name>` defining steps
to perform the job and potentially listing artifacts produced or required.

Example:

```
job build:
  shell: |
    ./configure
    make
  artifact bin:
    path: build/example

job test:
  uses:
    - build.bin
  shell: |
    ./build/example test
```

Each job is a map with the following attributes:

`shell`
: Shell script to perform the job.

`artifact <name>`
: Defines artifact `<name>` produced by this job. Is itself defined as a dictionary.

`artifact <name>.path`
: Path to the produced artifact file, relative to the root of the project.

`uses`
: List of artifact required for this job, in the form `<job>.<artifact>`.

`checkout`
: List of repositories to checkout into the work directory before starting
  the job (see below). If not specified, the default repository (containing
  the YAML file) will be checked out to the root of the work directory.

`publish`
: List of artifacts (produced by this or other jobs) to publish to specified
  destinations (see below).


Usage
-----

To run jobs for a git commit range:
```
minici run <commit>..<commit>
```

or:
```
minici run --range=<commit>..<commit>
```

To run jobs for commits that are in local `<branch>`, but not yet in its upstream:
```
minici run --since-upstream=<branch>
```

For current branch, the name can be omitted:
```
minici run
```

To run selected jobs with the current working tree, including uncommitted
changes, list the job names on command line:
```
minici run <job name> [<job name> ...]
```

To watch changes on given `<branch>` and run jobs for each new commit:
```
minici run --new-commits-on=<branch>
```

To watch new tags and run jobs for each tag matching given pattern:
```
minici run --new-tags=<pattern>
```

The above options `--range`, `--since-upstream`, etc can be arbitrarily combined.


Explicit script file
--------------------

Normally, `minici` uses the `minici.yaml` file from the repository in the current working directory;
the path to a repository used can also be specified on the command line before the command (e.g. `run`).
That path must contain a slash character to disambiguate it from a command name, e.g.:
```
minici ../path/to/some/other/repo run some_job
```

Whether using the implicit repository or one specified on the command line, when
executing jobs for a range of commits, `minici` uses the `minici.yaml` file
version from each individual commit. So for example, running:
```
minici run HEAD~5..HEAD
```
can execute different jobs for each commit, if the job file was changed in each of the last five commits.

To force usage of particular version of `minici.yaml` file, explicit path to that file can be given:
```
minici ./minici.yaml run HEAD~5..HEAD
```
In this case, the jobs specified in the current version of `minici.yaml` will
be used for all of the five commits.


Additional repositories and checkouts
-------------------------------------

Jobs can also use repositories other than the default one (which contains the `minici.yaml` file).
The additional repositories need to be declared at the top level of the `minici.yaml` file as:
```
repo <name>:
    path: <path/to/repository> (optional, only relevant for explicit script file)
```

The path of a repository `<name>` can be given (or overridden) on command line by passing argument in the form `--repo=<name>:<path>` before the command name.
To explicitly set what repositories to check out, set the list in the `checkout` element of the job definition:

```
job some_job:
  checkout:
    - repo: some_repo
      dest: destination/path
  ...
```

The elements of the `checkout` list are dictionaries with following elements:

`repo`
: Name of the repo to checkout from. If not given, the default repo (containing the `minici.yaml` file) is used.

`dest`
: Path within the work directory to use for the checkout. If not given, the root of the work directory is used.

`subtree`
: If provided, checkout only given subtree of the repository. If the given path does not exist within a particular commit, the checkout and the whole job fail.

All checkouts are performed before running the script given in the `shell` element.
If no `checkout` list is given, the whole default repo is checked out to the root of the work directory.


Destinations
------------

Apart from a recipe, a job can also contain instructions to publish artifacts to given destinations.
The artifacts can be produced either by the publishing job or other jobs, and the destination must be declared at the top of the job file:

```
destination <name>:
  url: <destination url> (optional, only relevant for explicit script file)
```

**Note**: only local filesystem path is currently supported as `url`.

The URL of a destination `<name>` can be given (or overridden) on command line by passing argument in the form `--destination=<name>:<url>` before the command name.
The artifacts to publish are then given in the `publish` list of a job definition:
```
job publish_something:
  publish:
    - to: some_destination
      artifact: other_job.some_artifact
```

Elements of the `publish` list are dictionaries with following elements:

`to`
: Name of the destinations to publish to.

`artifact`
: Artifact to publish—either `<artifact-name>` if produced by this job, or `<job-name>.<artifact-name>` if produced by other job.

`path`
: Path within the destination to publish this artifact to. If not given, artifact path from the work directory is used.
