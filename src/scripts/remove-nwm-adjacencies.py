#!/usr/bin/env python3
#
# A one-time script for removing manually written adjacency code for NWM which becomes redundant with the DLL.

import os
from enum import Enum

class State(Enum):
    INIT = 0
    # BASEOVERRIDE = 1
    ADJACENCY = 2
    SECTIONENDED = 3
    SECTIONENDEDSKIP = 4

for parent, dirs, files in os.walk("Controller/RUL2/08_NWM"):
    for fname in files:
        if fname in ["00_Orthogonal_NWM.txt", "02_Diagonal_NWM.txt"]:
            continue  # base orthogonals and diagonals must be kept

        p = os.path.join(parent, fname)
        p_tmp = os.path.join(parent, f"{fname}.tmp")
        state = State.INIT

        with open(p, encoding='utf-8') as file:
            with open (p_tmp, 'w', encoding='utf-8') as file_tmp:

                for line in file:
                    if any(heading in line for heading in ["---OxO", "---OxD", "---DxO", "---DxD"]):
                        state = State.ADJACENCY
                    elif any(heading in line for heading in ["---END OxO", "---END OxD", "---END DxO", "---END DxD"]):
                        state = State.SECTIONENDEDSKIP
                    elif "---END L" in line:
                        state = State.SECTIONENDED
                    elif any(heading in line for heading in [
                            "---Other Base Crossings---",
                            "---Other Network Transitions---",
                            " Transition Hooks---",
                        ]):
                        state = State.SECTIONENDED
                    else:
                        pass

                    if state == State.ADJACENCY:
                        pass
                    elif state == State.SECTIONENDEDSKIP:
                        state = State.SECTIONENDED  # and skip writing
                    else:
                        file_tmp.write(line)

        if state == State.INIT:
            os.remove(p_tmp)
        else:
            os.replace(p_tmp, p)
            if state != State.SECTIONENDED:
                print(f"Terminated with unexpected state in file {fname}")
