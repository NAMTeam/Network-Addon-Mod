There are two advanced Avenue INRUL files.  RUL0E_Avenue_Advanced.rul is the default, RUL0E_Avenue_Advanced_TurningLanes.rul is used in the legacy Avenue Auto Turn Lanes plugin.  

Since it is installed optionally and there is no compilation routine in the installer, the turning lanes rul file contains all the INRUL from the original file, plus the turn lanes INRUL.

The turning lanes INRUL is a fully specified #roadrules# INRUL, whereas the main file makes use of transmogrify.

THEREFORE, if changes are to be made to the base avenue advanced INRUL, they must also be made in the turn lanes INRUL file, in non-transmogrify format.  An updated copy of RUL0E should be placed into the legacy turn lanes .dat file currently located at

installation\2 Additional Network Features\Road, One-Way Road, and Avenue\!Legacy Auto Avenue Turn Lanes\NetworkAddonMod_TurningLanes_Avenues_Plugin_INRULs.dat