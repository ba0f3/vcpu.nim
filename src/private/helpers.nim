import common

template LOWORD*(d: DWORD): WORD = d.DWORD
template HIWORD*(d: DWORD): WORD = (d shr 8).DWORD
template LOBYTE*(w: WORD): BYTE = w.BYTE
template HIBYTE*(w: WORD): WORD = (w shr 4).BYTEx

converter opcode2byte*(o: OpCode): BYTE = o.BYTE
converter regs2byte*(r: Regs): BYTE = r.BYTE