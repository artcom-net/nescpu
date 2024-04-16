import enum

from nescpu.mapper import Mapper0


class Version(enum.Enum):
    INES = 'iNES'
    NES20 = 'NES 2.0'


class Mirroring(enum.IntEnum):
    HORIZONTAL = 0
    VERTICAL = 1


class ConsoleType(enum.IntEnum):
    FAMILY = 0
    VS_SYSTEM = 1
    PLAYCHOICE10 = 2
    EXTENDED = 3


class TVSystem(enum.IntEnum):
    NTSC = 0
    PAL = 1


class Cartridge:

    _MAPPERS = {
        0: Mapper0
    }

    def __init__(self, rom: bytes):
        self._version = None
        self._prg_size = 0
        self._chr_size = 0
        # Flags 6
        self._mirroring = None
        self._has_battery_ram = False
        self._has_trainer = False
        self._ignore_mirroring = False
        self._mapper_number = None
        # Flags 7
        self._console_type = None
        # Flags 8
        self._prg_ram_size = 0
        # Flags 9
        self._tv_system = None
        self._is_dual_tv_system = False
        self._has_prg_ram = False
        self._has_bus_conflicts = False

        self._prg_rom = b''
        self._chr_rom = b''
        self._mapper = None

        self._parse_rom(rom)

    def read(self, address: int) -> int:
        return self._mapper.read(address)

    def write(self, address: int, data: int):
        ...

    def _parse_rom(self, rom: bytes) -> None:
        self._version = self._parse_version(rom)
        if self._version is Version.INES:
            parser = self._parse_ines_rom
        else:
            parser = self._parse_nes20_rom
        parser(rom)
        return None

    def _parse_version(self, rom: bytes) -> Version:
        if rom[:4] != b'NES\x1A':
            raise ValueError('invalid header value')
        if rom[7] & 0xC == 0x8:
            return Version.NES20
        return Version.INES

    def _parse_ines_rom(self, rom: bytes) -> None:
        self._prg_size = rom[4]
        self._chr_size = rom[5]

        flags6 = rom[6]
        self._mirroring = Mirroring(flags6 & 1)
        self._has_battery_ram = bool(flags6 & 2)
        self._has_trainer = bool(flags6 >> 2 & 1)
        self._ignore_mirroring = bool(flags6 >> 3 & 1)
        self._mapper_number = flags6 >> 4

        flags7 = rom[7]
        self._console_type = ConsoleType(flags7 & 3)
        self._mapper_number |= flags7 & 0xF0
        # Size of PRG RAM in 8 KB units (Value 0 infers 8 KB for compatibility)
        self._prg_ram_size = rom[8]

        flags9 = rom[9]
        self._tv_system = TVSystem(flags9 & 1)

        flags10 = rom[10]
        tv_system_id = flags10 & 3
        if tv_system_id in (1, 3):
            self._is_dual_tv_system = True
        self._has_prg_ram = bool(flags10 >> 4 & 1)
        self._has_bus_conflicts = bool(flags10 >> 5 & 1)

        # TODO: magic numbers..
        self._prg_rom = bytearray(rom[16: 16 + 16384 * self._prg_size])
        self._chr_rom = bytearray(rom[16 + 16384 * self._prg_size:])
        self._mapper = self._MAPPERS.get(self._mapper_number)(self._prg_rom,
                                                              self._chr_rom)
        if not self._mapper:
            raise ValueError(f'unknown mapper index: {self._mapper_number}')

    def _parse_nes20_rom(self, rom: bytes) -> None:
        ...
