MiniCI
======

MiniCI runs jobs defined in the `minici.yaml` file in the root of the project
(on the same level as the `.git` directory). With that, `minici` invocation can
execute the jobs for local commits that are not yet in upstream (remote) branch
or for any manually given commit range.


Job definition
--------------

The top-level elements of the YAML file are `job <name>` defining steps to
perform the job and potentially listing artifacts produced or required.

Example:

```
job build:
  shell:
    - make
  artifact bin:
    path: build/example

job test:
  uses:
    - build.bin
  shell:
    - ./build/example test
```

Each job is a map with the following attributes:

`shell`
: List of shell commands to perform the job

`artifact <name>` (optional)
: Defines artifact `<name>` produced by this job. Is itself defined as a dictionary.

`artifact <name>.path`
: Path to the produced artifact file, relative to the root of the project.

`uses` (optional)
: List of artifact required for this job, in the form `<job>.<artifact>`.


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
minici run <branch>
```
or:
```
minici run --since-upstream=<branch>
```

For current branch, the name can be omitted:
```
minici run
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
