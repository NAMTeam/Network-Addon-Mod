There are two advanced Avenue INRUL files contained in the NAM.  The main one is included with all the other INRUL files.  The other one is used in the legacy Avenue Auto Turn Lanes plugin.

Since it is installed optionally and there is no compilation routine in the installer, the turning lanes rul file contains all the INRUL from the original file, plus the turn lanes INRUL.

Both RUL0E files are fully specified #roadrules# INRUL files rather than making use of transmogrify in order to avoid some mirroring problems.

THEREFORE, if changes are to be made to the base avenue advanced INRUL, then also an updated copy of RUL0E should be placed into the legacy turn lanes .dat file currently located at

installation\2 Additional Network Features\Road, One-Way Road, and Avenue\!Legacy Auto Avenue Turn Lanes\NetworkAddonMod_TurningLanes_Avenues_Plugin_INRULs.dat
