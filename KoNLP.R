##### java setting on mac ########

# 0. dylib 위치
paste0(system2('/usr/libexec/java_home', stdout = TRUE), '/jre/lib/server/libjvm.dylib')

# 1. load dylib
dyn.load(paste0(system2('/usr/libexec/java_home', stdout = TRUE), 
                '/jre/lib/server/libjvm.dylib'))

