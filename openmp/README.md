# OpenMP


## Error

You sometimes get the following error

dyld: Library not loaded: @rpath/libiomp5.dylib


Use this command: `source /opt/intel/bin/compilervars.sh intel64`

* `omp_set_dynamic(.false.)

In this project, dynamic adjustment is disable.
