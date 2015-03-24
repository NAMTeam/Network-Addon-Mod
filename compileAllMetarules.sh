#!/bin/sh
find Controller -type f \( -name "*_MANAGED.rul" -or -name "*_MANAGED.txt" \) -exec /bin/rm {} \+ &&
sbt run
