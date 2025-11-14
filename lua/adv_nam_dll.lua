--#-package:2A504B00# -- package signature
----------------------------------------------------------------------
-- This file defines advisor messages related to the NAM DLL.

-- Note that this Lua file is not intended for distribution with the DLL itself, but for distribution with the NAM DBPF files to ensure compatibility between NAM and DLL.
nam_dll_version_expected = "1.2.3"  -- needs to be updated whenever a new DLL version is released

local _cached_result = nil
function is_nam_dll_correct()
    -- (When this script is first executed, the `nam_dll_version` is still `nil`, but it gets defined before the trigger conditions are evaluated.)
    local version = rawget(globals(), "nam_dll_version")
    if (version == nil) then
        return false
    elseif (_cached_result ~= nil) then
        return _cached_result
    else
        if (version == nam_dll_version_expected) then
            _cached_result = true
        else
            -- check semantic versions to allow a patch level higher than expected by this Lua script
            local semver_pattern = "^(%d+%.%d+)%.(%d+)"
            local i, _, v12, v3 = string.find(version, semver_pattern)
            local j, _, e12, e3 = string.find(nam_dll_version_expected, semver_pattern)
            if (i == nil or j == nil) then
                _cached_result = false
            else
                _cached_result = (v12 == e12 and tonumber(v3) >= tonumber(e3))
            end
        end
        return _cached_result
    end
end

------------ Advice record ----
a = create_advice_transportation('2a504ba0')
a.trigger  = "not is_nam_dll_correct()"
a.title = [[text@2a504ba0  Traffic Planners Have Lost The Plot]]
a.message = [[text@2a504ba1  They could not find the correct NAM DLL file in your Plugins. Required NAM DLL version: #nam_dll_version_expected#, Installed NAM DLL version: #nam_dll_version or 'not installed'#]]
a.once = 0  -- must be 0, since 1 means once per city (saving after closing the message would prevent message from ever appearing again, even after restarting the game or upgrading to a newer DLL version – unless the GUID is changed for every new DLL version)
a.persist = 1  -- irrelevant as trigger is not random (after game is restarted with correct DLL, the trigger seems to be evaluated anyway, so message disappears)
a.frequency = 90  -- days
a.timeout = tuning_constants.ADVICE_TIMEOUT_SHORT
a.no_timeout = 1  -- message stays active in background after closing popup, so the popup doesn't reappear every few months
a.priority = tuning_constants.ADVICE_PRIORITY_URGENT
a.mood = advice_moods.BAD_JOB

------------ Advice record ----
-- for debugging only
-- a = create_advice_transportation('2a504b90')
-- a.trigger  = "is_nam_dll_correct()"
-- a.title = [[Debug: Traffic Planners On The Right Track]]
-- a.message = [[The correct version of the NAM DLL is installed: #nam_dll_version or 'not installed'#]]
-- a.once = 0
-- a.persist = 1
-- a.frequency = 90
-- a.timeout = tuning_constants.ADVICE_TIMEOUT_SHORT
-- a.no_timeout = 1
-- a.priority = tuning_constants.ADVICE_PRIORITY_URGENT
-- a.mood = advice_moods.GREAT_JOB
