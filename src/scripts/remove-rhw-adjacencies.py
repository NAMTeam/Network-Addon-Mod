#!/usr/bin/env python3
#
# A one-time script for removing manually written adjacency code for RHW which becomes redundant with the DLL.

import os
from enum import Enum

class State(Enum):
    INIT = 0
    BASEOVERRIDE = 1
    ADJACENCY = 2
    SECTIONENDED = 3

for parent, dirs, files in os.walk("Controller/RUL2/07_RHW/Sec7b_BaseNetwork"):
    for fname in files:
        if fname in ["00_Orthogonal.txt", "02_Diagonal.txt"]:
            continue  # base orthogonals and diagonals must be kept

        p = os.path.join(parent, fname)
        p_tmp = os.path.join(parent, f"{fname}.tmp")
        state = State.INIT

        with open(p, encoding='utf-8') as file:
            with open (p_tmp, 'w', encoding='utf-8') as file_tmp:

                for line in file:
                    if line.startswith(";00 to 00") or line.startswith(";02 to 02"):
                        state = State.BASEOVERRIDE
                    elif "---END" in line:
                        state = State.SECTIONENDED
                    elif state == State.BASEOVERRIDE and (line.startswith(";00 to ") or line.startswith(";02 to ")):
                        state = State.ADJACENCY
                    else:
                        pass

                    if state != State.ADJACENCY:
                        file_tmp.write(line)

        if state == State.INIT:
            os.remove(p_tmp)
        else:
            os.replace(p_tmp, p)
            if state != State.SECTIONENDED:
                print(f"Terminated with unexpected state in file {fname}")
