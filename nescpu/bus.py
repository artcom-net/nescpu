from nescpu.cartridge import Cartridge

_2KB = 2048


class CPUMemoryBus:

    def __init__(self, cartridge: Cartridge):
        self._ram = bytearray(_2KB)
        self._cartridge = cartridge

    def read(self, address: int) -> int:
        if 0x0000 <= address <= 0x1FFF:
            return self._ram[address & 0x07FF]
        if 0x2000 <= address <= 0x3FFF:
            # PPU registers
            return 0
        if 0x4000 <= address <= 0x4017:
            # APU and I/O registers
            return 0
        if 0x4018 <= address <= 0x401F:
            # APU and I/O functionality that is normally disabled
            return 0
        if 0x4020 <= address <= 0xFFFF:
            # PRG ROM, PRG RAM and mapper registers
            return self._cartridge.read(address)
        raise ValueError(f'address out of range: {address:04X}')

    def write(self, address: int, data: int) -> None:
        if 0x0000 <= address <= 0x1FFF:
            self._ram[address & 0x07FF] = data
            return None
        if 0x2000 <= address <= 0x3FFF:
            # PPU registers
            return None
        if address == 0x4014:
            # DMA
            return None
        if 0x4000 <= address <= 0x4017:
            # APU and I/O registers
            return None
        if 0x4018 <= address <= 0x401F:
            # APU and I/O functionality that is normally disabled
            return None
        if 0x4020 <= address <= 0xFFFF:
            # PRG ROM, PRG RAM and mapper registers
            return self._cartridge.write(address, data)
        raise ValueError(f'address out of range: {address:04X}')
