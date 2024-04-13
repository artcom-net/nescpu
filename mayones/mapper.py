

class Mapper0:

    def __init__(self, prg_rom: bytearray, chr_rom: bytearray):
        self._prg_rom = prg_rom
        self._chr_rom = chr_rom

    def read(self, address: int) -> int:
        if 0x8000 <= address <= 0xFFFF:
            return self._prg_rom[address & 0x3FFF]
        if 0x0000 <= address <= 0x1FFF:
            return self._chr_rom[address]
        return 0
