import common, strutils

template LOWORD*(d: DWORD): WORD = d.DWORD
template HIWORD*(d: DWORD): WORD = (d shr 8).DWORD
template LOBYTE*(w: WORD): BYTE = w.BYTE
template HIBYTE*(w: WORD): WORD = (w shr 8).BYTE

converter opcode2byte*(o: OpCode): BYTE = o.BYTE
converter regs2byte*(r: Regs): BYTE = r.BYTE


proc r*(idx: uint8): tuple[reg: BYTE, size: BYTE] =
  var
    reg = idx mod 6
  result = (reg, 0'u8)

proc hexdump*(data: cstring, length: int) =
  var
    ascii: array[17, char]
    i, j: int
  ascii[16] = '\0'
  for i in 0..<length:
    stdout.write toHex(data[i].BYTE)
    stdout.write " "
    if data[i] >= ' ' and data[i] <= '~':
      ascii[i mod 16] = data[i]
    else:
      ascii[i mod 16] = '.'
    if (i+1) mod 16 == 0:
      echo " | ", cast[cstring](addr ascii)
    elif (i+1) == length:
      ascii[(i+1) mod 16] = '\0'
      if (i+1) mod 16 <= 8:
        stdout.write " "

      j = (i+1) mod 16
      while j < 16:
        stdout.write "   "
        inc(j)
      echo "| ", cast[cstring](addr ascii)




  #[

void hexdump(const void* data, size_t size) {
	char ascii[17];
	size_t i, j;
	ascii[16] = '\0';
	for (i = 0; i < size; ++i) {
		printf("%02X ", ((unsigned char*)data)[i]);
		if (((unsigned char*)data)[i] >= ' ' && ((unsigned char*)data)[i] <= '~') {
			ascii[i % 16] = ((unsigned char*)data)[i];
		} else {
			ascii[i % 16] = '.';
		}
		if ((i+1) % 8 == 0 || i+1 == size) {
			printf(" ");
			if ((i+1) % 16 == 0) {
				printf("|  %s \n", ascii);
			} else if (i+1 == size) {
				ascii[(i+1) % 16] = '\0';
				if ((i+1) % 16 <= 8) {
					printf(" ");
				}
				for (j = (i+1) % 16; j < 16; ++j) {
					printf("   ");
				}
				printf("|  %s \n", ascii);
			}
		}
	}
}
  ]#