Network-Addon-Mod
=================

[![Build Status](https://travis-ci.org/NAMTeam/Network-Addon-Mod.svg?branch=master)](https://travis-ci.org/NAMTeam/Network-Addon-Mod)

Network Addon Mod for SimCity 4 Deluxe/Rush Hour @ http://sc4devotion.com/forums/index.php?board=90.0


Instructions for compiling metarules
------------------------------------

First, make sure you have installed SBT and JDK 1.7 or above. Then, execute the script "compileAllMetarules.sh"
in this directory, which generates proper RUL files and drops them in specific locations in
the "Controller" directory. This script needs to be executed every time the metarules are
updated or you checkout a different branch (which may update the metarules) in order to
assert that the generated files are always up-to-date.

Moreover, the script builds the INRUL files which can be found in the directory "target/INRULs"
after running the script.
