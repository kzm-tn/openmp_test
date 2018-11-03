# OpenMP

In advance, I'm sorry for my English.

## Construct

* `!$OMP parallel`
* `!$OMP do`
* `!$OMP parallel do`
* `!$OMP parallel do reduction(+:...)`
* `!$OMP critical`
* `!$OMP barrier`
* `!$OMP single`
* `!$OMP ordered`

## Error

You sometimes get the following error(macOS)

dyld: Library not loaded: @rpath/libiomp5.dylib


Use this command: `source /opt/intel/bin/compilervars.sh intel64`

* `omp_set_dynamic(.false.)

In this project, dynamic adjustment is disable.


## Barrier construct

Loop construct, sections construct, single construct, and workshare construct don't need barrier construct. These constucts synchronize each thread at the end of the construct implicitly.
