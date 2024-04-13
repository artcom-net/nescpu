import dataclasses
import enum
import typing

from mayones.bus import CPUMemoryBus

ADC = 'ADC'
AND = 'AND'
ASL = 'ASL'
BCC = 'BCC'
BCS = 'BCS'
BEQ = 'BEQ'
BIT = 'BIT'
BMI = 'BMI'
BNE = 'BNE'
BPL = 'BPL'
BRK = 'BRK'
BVC = 'BVC'
BVS = 'BVS'
CLC = 'CLC'
CLD = 'CLD'
CLI = 'CLI'
CLV = 'CLV'
CMP = 'CMP'
CPX = 'CPX'
CPY = 'CPY'
DEC = 'DEC'
DEX = 'DEX'
DEY = 'DEY'
EOR = 'EOR'
INC = 'INC'
INX = 'INX'
INY = 'INY'
JMP = 'JMP'
JSR = 'JSR'
LDA = 'LDA'
LDX = 'LDX'
LDY = 'LDY'
LSR = 'LSR'
NOP = 'NOP'
ORA = 'ORA'
PHA = 'PHA'
PHP = 'PHP'
PLA = 'PLA'
PLP = 'PLP'
ROL = 'ROL'
ROR = 'ROR'
RTI = 'RTI'
RTS = 'RTS'
SBC = 'SBC'
SEC = 'SEC'
SED = 'SED'
SEI = 'SEI'
STA = 'STA'
STX = 'STX'
STY = 'STY'
TAX = 'TAX'
TAY = 'TAY'
TSX = 'TSX'
TXA = 'TXA'
TXS = 'TXS'
TYA = 'TYA'


class IllegalOpcodeError(Exception):

    def __init__(self, opcode: int):
        self.opcode = opcode


class Flag(enum.IntFlag):
    CARRY = enum.auto()
    ZERO = enum.auto()
    INTERRUPT = enum.auto()
    DECIMAL = enum.auto()
    BREAK = enum.auto()
    UNUSED = enum.auto()
    OVERFLOW = enum.auto()
    NEGATIVE = enum.auto()


class AddressMode(enum.Enum):
    ACCUMULATOR = enum.auto()
    IMPLIED = enum.auto()
    IMMEDIATE = enum.auto()
    RELATIVE = enum.auto()
    ZEROPAGE = enum.auto()
    ZEROPAGE_X = enum.auto()
    ZEROPAGE_Y = enum.auto()
    ABSOLUTE = enum.auto()
    ABSOLUTE_X = enum.auto()
    ABSOLUTE_Y = enum.auto()
    INDIRECT = enum.auto()
    INDIRECT_X = enum.auto()
    INDIRECT_Y = enum.auto()


@dataclasses.dataclass(frozen=True, slots=True)
class InstructionContext:
    raw_operand: int | None
    operand: int | None
    address: int | None


@dataclasses.dataclass(frozen=True, slots=True)
class Instruction:
    mnemonic: str
    cycles: int
    address_mode: AddressMode
    get_context: typing.Callable[[], InstructionContext]
    check_page_cross: bool
    execute: typing.Callable[[], None]


@dataclasses.dataclass(frozen=True, slots=True)
class TraceEntry:
    pc: int
    opcode: int
    mnemonic: str
    operand: int | None
    a: int
    x: int
    y: int
    flags: int
    sp: int
    cycles: int

    def __repr__(self):
        operand = str(None) if self.operand is None else f'{self.operand:04X}'
        return f'{self.__class__.__name__}(' \
               f'pc={self.pc:04X}, ' \
               f'opcode={self.opcode:02X}, ' \
               f'mnemonic={self.mnemonic}, ' \
               f'operand={operand}, ' \
               f'a={self.a:02X}, ' \
               f'x={self.x:02X}, ' \
               f'y={self.y:02X}, ' \
               f'flags={self.flags:02X}, ' \
               f'sp={self.sp:02X}, ' \
               f'cycles={self.cycles}' \
               f')'

    def __str__(self):
        operand = '' if self.operand is None else f'{self.operand:04X}'
        return f'{self.pc:04X} {self.opcode:02X} {self.mnemonic:>4} ' \
               f'{operand:<8} A={self.a:02X} X={self.x:02X} Y={self.y:02X} ' \
               f'P={self.flags:02X} SP={self.sp:02X} CYC={self.cycles}'


class CPU:

    _STACK_BASE_ADDR = 0x0100
    _NMI_VECTOR_ADDR = 0xFFFA
    _RESET_VECTOR_ADDR = 0xFFFC
    _IRQ_VECTOR_ADDR = 0xFFFE

    __slots__ = ('_a', '_x', '_y', '_sp', '_flags', '_pc', '_bus',
                 '_curr_cycles', '_total_cycles', '_instruction', '_context',
                 '_page_crossed', '_instructions_set')

    def __init__(self, bus: CPUMemoryBus):
        self._a = 0x00
        self._x = 0x00
        self._y = 0x00
        self._sp = 0x00
        self._flags = 0x00
        self._pc = 0x0000

        self._bus = bus
        self._curr_cycles = 0
        self._total_cycles = 0
        self._instruction: Instruction | None = None
        self._context: InstructionContext | None = None
        self._page_crossed = False
        self._instructions_set = self._init_instructions()

        self.reset()

    def reset(self, pc: int | None = None) -> None:
        self._a = 0x00
        self._x = 0x00
        self._y = 0x00
        self._sp = 0xFD
        if pc is not None:
            self._pc = pc
        else:
            self._pc = self._bus.read(self._RESET_VECTOR_ADDR) | \
                       self._bus.read(self._RESET_VECTOR_ADDR + 1) << 8
        self._flags = Flag.INTERRUPT | Flag.UNUSED
        self._total_cycles = 7
        return None

    def tick(self) -> int:
        self._curr_cycles = 0
        opcode = self._bus.read(self._pc)
        self._pc += 1
        self._instruction = self._instructions_set.get(opcode)
        if not self._instruction:
            raise IllegalOpcodeError(opcode)
        self._context = self._instruction.get_context()
        self._instruction.execute()
        if self._instruction.check_page_cross and self._page_crossed:
            self._curr_cycles += 1
            self._page_crossed = False
        self._curr_cycles += self._instruction.cycles
        self._total_cycles += self._curr_cycles
        return self._curr_cycles

    def trace_tick(self) -> TraceEntry:
        pc = self._pc
        a = self._a
        x = self._x
        y = self._y
        sp = self._sp
        flags = self._flags
        total_cycles = self._total_cycles
        self.tick()
        opcode = self._bus.read(pc)
        return TraceEntry(
            pc=pc,
            opcode=opcode,
            mnemonic=self._instruction.mnemonic,
            operand=self._context.raw_operand,
            a=a,
            x=x,
            y=y,
            flags=flags,
            sp=sp,
            cycles=total_cycles
        )

    def _is_page_crossed(self, address1: int, address2: int) -> bool:
        return address1 & 0xFF00 != address2 & 0xFF00

    def _read_address_around_page(self, address: int) -> int:
        pointer = self._bus.read(address)
        if self._is_page_crossed(address, address + 1):
            pointer |= self._bus.read(address & 0xFF00) << 8
        else:
            pointer |= self._bus.read(address + 1) << 8
        return pointer

    def _get_accumulator_context(self) -> InstructionContext:
        return InstructionContext(None, self._a, None)

    def _get_implied_context(self) -> InstructionContext:
        return InstructionContext(None, None, None)

    def _get_immediate_context(self) -> InstructionContext:
        self._pc += 1
        operand = self._bus.read(self._pc - 1)
        return InstructionContext(operand, operand, 0)

    def __resolve_zeropage_context(self, index: int) -> InstructionContext:
        operand = self._bus.read(self._pc)
        self._pc += 1
        address = (operand + index) & 0xFF
        resolved_operand = self._bus.read(address)
        return InstructionContext(operand, resolved_operand, address)

    def _get_zeropage_context(self) -> InstructionContext:
        return self.__resolve_zeropage_context(0)

    def _get_zeropage_x_context(self) -> InstructionContext:
        return self.__resolve_zeropage_context(self._x)

    def _resolve_zeropage_y_context(self) -> InstructionContext:
        return self.__resolve_zeropage_context(self._y)

    def __resolve_absolute_context(self, index: int) -> InstructionContext:
        operand = self._bus.read(self._pc) | self._bus.read(self._pc + 1) << 8
        self._pc += 2
        effective_address = (operand + index) & 0xFFFF
        self._page_crossed = self._is_page_crossed(operand, effective_address)
        return InstructionContext(operand, self._bus.read(effective_address),
                                  effective_address)

    def _get_absolute_context(self) -> InstructionContext:
        return self.__resolve_absolute_context(0)

    def _get_absolute_x_context(self) -> InstructionContext:
        return self.__resolve_absolute_context(self._x)

    def _get_absolute_y_context(self) -> InstructionContext:
        return self.__resolve_absolute_context(self._y)

    def _get_indirect_context(self) -> InstructionContext:
        operand = self._bus.read(self._pc) | self._bus.read(self._pc + 1) << 8
        self._pc += 2
        effective_address = self._read_address_around_page(operand)
        return InstructionContext(operand,
                                  self._bus.read(effective_address),
                                  effective_address)

    def _get_indirect_x_context(self) -> InstructionContext:
        operand = self._bus.read(self._pc)
        self._pc += 1
        zeropage_addr = (operand + self._x) & 0xFF
        effective_address = self._read_address_around_page(zeropage_addr)
        return InstructionContext(operand,
                                  self._bus.read(effective_address),
                                  effective_address)

    def _get_indirect_y_context(self) -> InstructionContext:
        operand = self._bus.read(self._pc)
        self._pc += 1
        base_addr = self._read_address_around_page(operand)
        effective_address = (base_addr + self._y) & 0xFFFF
        self._page_crossed = self._is_page_crossed(base_addr,
                                                   effective_address)
        return InstructionContext(operand,
                                  self._bus.read(effective_address),
                                  effective_address)

    def _get_relative_context(self) -> InstructionContext:
        operand = self._bus.read(self._pc)
        self._pc += 1
        return InstructionContext(operand, operand, 0)

    def _calc_signed_address_offset(self, offset: int) -> int:
        return offset - 0x0100 if offset & 0x80 else offset

    def _resolve_stack_address(self) -> int:
        return self._STACK_BASE_ADDR | self._sp

    def _push_stack(self, data: int) -> None:
        # TODO: check stack overflow
        self._bus.write(self._resolve_stack_address(), data)
        self._sp -= 1
        return None

    def _pop_stack(self) -> int:
        self._sp += 1
        data = self._bus.read(self._resolve_stack_address())
        return data

    def _set_flag(self, flag: Flag, value: int) -> None:
        if value:
            self._flags |= flag
        else:
            self._flags &= ~flag

    def _set_nz_flags(self, data: int) -> None:
        self._set_flag(Flag.ZERO, int(data == 0))
        self._set_flag(Flag.NEGATIVE, int(data >> 7 & 1))

    def _store(self, address: int, data: int) -> None:
        if self._instruction.address_mode is AddressMode.ACCUMULATOR:
            self._a = data
        else:
            self._bus.write(address, data)

    def _brk(self) -> None:
        return_addr = self._pc + 1
        self._push_stack(return_addr >> 8)
        self._push_stack(return_addr & 0xFF)
        self._push_stack(self._flags | Flag.BREAK)
        self._flags |= Flag.INTERRUPT
        self._pc = (
            self._bus.read(self._IRQ_VECTOR_ADDR) |
            self._bus.read(self._IRQ_VECTOR_ADDR + 1) << 8
        )

    def _ora(self) -> None:
        self._a |= self._context.operand
        self._set_nz_flags(self._a)

    def _and(self) -> None:
        self._a &= self._context.operand
        self._set_nz_flags(self._a)

    def _eor(self) -> None:
        self._a ^= self._context.operand
        self._set_nz_flags(self._a)

    def _asl(self) -> None:
        result = self._context.operand << 1 & 0xFF
        self._set_flag(Flag.CARRY, self._context.operand >> 7)
        self._set_nz_flags(result)
        self._store(self._context.address, result)

    def _lsr(self) -> None:
        result = self._context.operand >> 1
        self._set_flag(Flag.CARRY, self._context.operand & 1)
        self._set_nz_flags(result)
        self._store(self._context.address, result)

    def _php(self) -> None:
        self._push_stack(self._flags | Flag.BREAK)

    def _plp(self) -> None:
        self._flags = self._pop_stack() & ~Flag.BREAK | Flag.UNUSED

    def _pha(self) -> None:
        self._push_stack(self._a)

    def _pla(self) -> None:
        self._a = self._pop_stack()
        self._set_nz_flags(self._a)

    def _clc(self) -> None:
        self._flags &= ~Flag.CARRY

    def _sec(self) -> None:
        self._flags |= Flag.CARRY

    def _cli(self) -> None:
        self._flags &= ~Flag.INTERRUPT

    def _sei(self) -> None:
        self._flags |= Flag.INTERRUPT

    def _clv(self) -> None:
        self._flags &= ~Flag.OVERFLOW

    def _cld(self) -> None:
        self._flags &= ~Flag.DECIMAL

    def _sed(self) -> None:
        self._flags |= Flag.DECIMAL

    def _bit(self) -> None:
        self._set_flag(Flag.ZERO, int(self._a & self._context.operand == 0))
        self._set_flag(Flag.OVERFLOW, self._context.operand >> 6 & 1)
        self._set_flag(Flag.NEGATIVE, self._context.operand >> 7)

    def _rol(self) -> None:
        rotated = (self._context.operand << 1
                   | self._flags & Flag.CARRY) & 0xFF
        self._set_flag(Flag.CARRY, self._context.operand >> 7)
        self._set_nz_flags(rotated)
        self._store(self._context.address, rotated)

    def _ror(self) -> None:
        rotated = (self._context.operand >> 1 |
                   (self._flags & Flag.CARRY) << 7) & 0xFF
        self._set_flag(Flag.CARRY, self._context.operand & 1)
        self._set_nz_flags(rotated)
        self._store(self._context.address, rotated)

    def _rti(self) -> None:
        self._flags = self._pop_stack() | Flag.UNUSED
        self._pc = self._pop_stack() | self._pop_stack() << 8

    def _rts(self) -> None:
        self._pc = (self._pop_stack() | self._pop_stack() << 8) + 1

    def _jmp(self) -> None:
        self._pc = self._context.address

    def __adc(self, operand: int) -> None:
        result = self._a + operand + (self._flags & Flag.CARRY)
        self._set_flag(Flag.CARRY, int(result > 0xFF))
        self._set_flag(
            Flag.OVERFLOW,
            int(((self._a ^ result) & (operand ^ result) & 0x80) >> 7)
        )
        self._set_nz_flags(result & 0xFF)
        self._a = result & 0xFF

    def _adc(self) -> None:
        self.__adc(self._context.operand)

    def _sbc(self) -> None:
        self.__adc(self._context.operand ^ 0xFF)

    def _sta(self) -> None:
        self._store(self._context.address, self._a)

    def _sty(self) -> None:
        self._store(self._context.address, self._y)

    def _stx(self) -> None:
        self._store(self._context.address, self._x)

    def _inx(self) -> None:
        self._x = (self._x + 1) & 0xFF
        self._set_nz_flags(self._x)

    def _dex(self) -> None:
        self._x = (self._x - 1) & 0xFF
        self._set_nz_flags(self._x)

    def _iny(self) -> None:
        self._y = (self._y + 1) & 0xFF
        self._set_nz_flags(self._y)

    def _dey(self) -> None:
        self._y = (self._y - 1) & 0xFF
        self._set_nz_flags(self._y)

    def _inc(self) -> None:
        result = (self._context.operand + 1) & 0xFF
        self._set_nz_flags(result)
        self._store(self._context.address, result)

    def _dec(self) -> None:
        result = (self._context.operand - 1) & 0xFF
        self._set_nz_flags(result)
        self._store(self._context.address, result)

    def _txa(self) -> None:
        self._a = self._x
        self._set_nz_flags(self._a)

    def _tya(self) -> None:
        self._a = self._y
        self._set_nz_flags(self._a)

    def _txs(self) -> None:
        self._sp = self._x

    def _tay(self) -> None:
        self._y = self._a
        self._set_nz_flags(self._y)

    def _tax(self) -> None:
        self._x = self._a
        self._set_nz_flags(self._x)

    def _tsx(self) -> None:
        self._x = self._sp
        self._set_nz_flags(self._x)

    def _ldx(self) -> None:
        self._x = self._context.operand
        self._set_nz_flags(self._x)

    def _ldy(self) -> None:
        self._y = self._context.operand
        self._set_nz_flags(self._y)

    def _lda(self) -> None:
        self._a = self._context.operand
        self._set_nz_flags(self._a)

    def __cmp(self, register: int) -> None:
        result = register - self._context.operand
        self._set_flag(Flag.CARRY, int(register >= self._context.operand))
        self._set_nz_flags(result)

    def _cpx(self) -> None:
        self.__cmp(self._x)

    def _cpy(self) -> None:
        self.__cmp(self._y)

    def _cmp(self) -> None:
        self.__cmp(self._a)

    def _branch(self, condition: bool) -> None:
        if not condition:
            return None
        self._curr_cycles += 1
        offset = self._context.operand
        address = self._pc + self._calc_signed_address_offset(offset)
        self._page_crossed = self._is_page_crossed(self._pc, address)
        self._pc = address

    def _bpl(self) -> None:
        self._branch(not (self._flags & Flag.NEGATIVE))

    def _bmi(self) -> None:
        self._branch(bool(self._flags & Flag.NEGATIVE))

    def _bvc(self) -> None:
        self._branch(not (self._flags & Flag.OVERFLOW))

    def _bvs(self) -> None:
        self._branch(bool(self._flags & Flag.OVERFLOW))

    def _bcc(self) -> None:
        self._branch(not (self._flags & Flag.CARRY))

    def _bcs(self) -> None:
        self._branch(bool(self._flags & Flag.CARRY))

    def _bne(self) -> None:
        self._branch(not (self._flags & Flag.ZERO))

    def _beq(self) -> None:
        self._branch(bool(self._flags & Flag.ZERO))

    def _jsr(self) -> None:
        return_address = self._pc - 1
        self._push_stack(return_address >> 8)
        self._push_stack(return_address & 0xFF)
        self._pc = self._context.address

    def _nop(self) -> None:
        return None

    def _init_instructions(self) -> dict[int, Instruction]:
        return {
            0x00: Instruction(BRK,
                              7,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._brk),
            0x01: Instruction(ORA,
                              6,
                              AddressMode.INDIRECT_X,
                              self._get_indirect_x_context,
                              False,
                              self._ora),
            0x05: Instruction(ORA,
                              3,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._ora),
            0x06: Instruction(ASL,
                              5,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._asl),
            0x08: Instruction(PHP,
                              3,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._php),
            0x09: Instruction(ORA,
                              2,
                              AddressMode.IMMEDIATE,
                              self._get_immediate_context,
                              False,
                              self._ora),
            0x0A: Instruction(ASL,
                              2,
                              AddressMode.ACCUMULATOR,
                              self._get_accumulator_context,
                              False,
                              self._asl),
            0x0D: Instruction(ORA,
                              4,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._ora),
            0x0E: Instruction(ASL,
                              6,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._asl),
            0x10: Instruction(BPL,
                              2,
                              AddressMode.RELATIVE,
                              self._get_relative_context,
                              False,
                              self._bpl),
            0x11: Instruction(ORA,
                              5,
                              AddressMode.INDIRECT_Y,
                              self._get_indirect_y_context,
                              True,
                              self._ora),
            0x15: Instruction(ORA,
                              4,
                              AddressMode.ZEROPAGE_X,
                              self._get_zeropage_x_context,
                              False,
                              self._ora),
            0x16: Instruction(ASL,
                              6,
                              AddressMode.ZEROPAGE_X,
                              self._get_zeropage_x_context,
                              False,
                              self._asl),
            0x18: Instruction(CLC,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._clc),
            0x19: Instruction(ORA,
                              4,
                              AddressMode.ABSOLUTE_Y,
                              self._get_absolute_y_context,
                              True,
                              self._ora),
            0x1D: Instruction(ORA,
                              4,
                              AddressMode.ABSOLUTE_X,
                              self._get_absolute_x_context,
                              True,
                              self._ora),
            0x1E: Instruction(ASL,
                              7,
                              AddressMode.ABSOLUTE_X,
                              self._get_absolute_x_context,
                              False,
                              self._asl),
            0x20: Instruction(JSR,
                              6,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._jsr),
            0x21: Instruction(AND,
                              6,
                              AddressMode.INDIRECT_X,
                              self._get_indirect_x_context,
                              False,
                              self._and),
            0x24: Instruction(BIT,
                              3,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._bit),
            0x25: Instruction(AND,
                              3,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._and),
            0x26: Instruction(ROL,
                              5,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._rol),
            0x28: Instruction(PLP,
                              4,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._plp),
            0x29: Instruction(AND,
                              2,
                              AddressMode.IMMEDIATE,
                              self._get_immediate_context,
                              False,
                              self._and),
            0x2A: Instruction(ROL,
                              2,
                              AddressMode.ACCUMULATOR,
                              self._get_accumulator_context,
                              False,
                              self._rol),
            0x2C: Instruction(BIT,
                              4,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._bit),
            0x2D: Instruction(AND,
                              4,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._and),
            0x2E: Instruction(ROL,
                              6,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._rol),
            0x30: Instruction(BMI,
                              2,
                              AddressMode.RELATIVE,
                              self._get_relative_context,
                              False,
                              self._bmi),
            0x31: Instruction(AND,
                              5,
                              AddressMode.INDIRECT_Y,
                              self._get_indirect_y_context,
                              True,
                              self._and),
            0x35: Instruction(AND,
                              4,
                              AddressMode.ZEROPAGE_X,
                              self._get_zeropage_x_context,
                              False,
                              self._and),
            0x36: Instruction(ROL,
                              6,
                              AddressMode.ZEROPAGE_X,
                              self._get_zeropage_x_context,
                              False,
                              self._rol),
            0x38: Instruction(SEC,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._sec),
            0x39: Instruction(AND,
                              4,
                              AddressMode.ABSOLUTE_Y,
                              self._get_absolute_y_context,
                              True,
                              self._and),
            0x3D: Instruction(AND,
                              4,
                              AddressMode.ABSOLUTE_X,
                              self._get_absolute_x_context,
                              True,
                              self._and),
            0x3E: Instruction(ROL,
                              7,
                              AddressMode.ABSOLUTE_X,
                              self._get_absolute_x_context,
                              False,
                              self._rol),
            0x40: Instruction(RTI,
                              6,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._rti),
            0x41: Instruction(EOR,
                              6,
                              AddressMode.INDIRECT_X,
                              self._get_indirect_x_context,
                              False,
                              self._eor),
            0x45: Instruction(EOR,
                              3,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._eor),
            0x46: Instruction(LSR,
                              5,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._lsr),
            0x48: Instruction(PHA,
                              3,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._pha),
            0x49: Instruction(EOR,
                              2,
                              AddressMode.IMMEDIATE,
                              self._get_immediate_context,
                              False,
                              self._eor),
            0x4A: Instruction(LSR,
                              2,
                              AddressMode.ACCUMULATOR,
                              self._get_accumulator_context,
                              False,
                              self._lsr),
            0x4C: Instruction(JMP,
                              3,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._jmp),
            0x4D: Instruction(EOR,
                              4,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._eor),
            0x4E: Instruction(LSR,
                              6,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._lsr),
            0x50: Instruction(BVC,
                              2,
                              AddressMode.RELATIVE,
                              self._get_relative_context,
                              False,
                              self._bvc),
            0x51: Instruction(EOR,
                              5,
                              AddressMode.INDIRECT_Y,
                              self._get_indirect_y_context,
                              True,
                              self._eor),
            0x55: Instruction(EOR,
                              4,
                              AddressMode.ZEROPAGE_X,
                              self._get_zeropage_x_context,
                              False,
                              self._eor),
            0x56: Instruction(LSR,
                              6,
                              AddressMode.ZEROPAGE_X,
                              self._get_zeropage_x_context,
                              False,
                              self._lsr),
            0x58: Instruction(CLI,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._cli),
            0x59: Instruction(EOR,
                              4,
                              AddressMode.ABSOLUTE_Y,
                              self._get_absolute_y_context,
                              True,
                              self._eor),
            0x5D: Instruction(EOR,
                              4,
                              AddressMode.ABSOLUTE_X,
                              self._get_absolute_x_context,
                              True,
                              self._eor),
            0x5E: Instruction(LSR,
                              7,
                              AddressMode.ABSOLUTE_X,
                              self._get_absolute_x_context,
                              False,
                              self._lsr),
            0x60: Instruction(RTS,
                              6,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._rts),
            0x61: Instruction(ADC,
                              6,
                              AddressMode.INDIRECT_X,
                              self._get_indirect_x_context,
                              False,
                              self._adc),
            0x65: Instruction(ADC,
                              3,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._adc),
            0x66: Instruction(ROR,
                              5,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._ror),
            0x68: Instruction(PLA,
                              4,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._pla),
            0x69: Instruction(ADC,
                              2,
                              AddressMode.IMMEDIATE,
                              self._get_immediate_context,
                              False,
                              self._adc),
            0x6A: Instruction(ROR,
                              2,
                              AddressMode.ACCUMULATOR,
                              self._get_accumulator_context,
                              False,
                              self._ror),
            0x6C: Instruction(JMP,
                              5,
                              AddressMode.INDIRECT,
                              self._get_indirect_context,
                              False,
                              self._jmp),
            0x6D: Instruction(ADC,
                              4,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._adc),
            0x6E: Instruction(ROR,
                              6,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._ror),
            0x70: Instruction(BVS,
                              2,
                              AddressMode.RELATIVE,
                              self._get_relative_context,
                              False,
                              self._bvs),
            0x71: Instruction(ADC,
                              5,
                              AddressMode.INDIRECT_Y,
                              self._get_indirect_y_context,
                              True,
                              self._adc),
            0x75: Instruction(ADC,
                              4,
                              AddressMode.ZEROPAGE_X,
                              self._get_zeropage_x_context,
                              False,
                              self._adc),
            0x76: Instruction(ROR,
                              6,
                              AddressMode.ZEROPAGE_X,
                              self._get_zeropage_x_context,
                              False,
                              self._ror),
            0x78: Instruction(SEI,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._sei),
            0x79: Instruction(ADC,
                              4,
                              AddressMode.ABSOLUTE_Y,
                              self._get_absolute_y_context,
                              True,
                              self._adc),
            0x7D: Instruction(ADC,
                              4,
                              AddressMode.ABSOLUTE_X,
                              self._get_absolute_x_context,
                              True,
                              self._adc),
            0x7E: Instruction(ROR,
                              7,
                              AddressMode.ABSOLUTE_X,
                              self._get_absolute_x_context,
                              False,
                              self._ror),
            0x81: Instruction(STA,
                              6,
                              AddressMode.INDIRECT_X,
                              self._get_indirect_x_context,
                              False,
                              self._sta),
            0x84: Instruction(STY,
                              3,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._sty),
            0x85: Instruction(STA,
                              3,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._sta),
            0x86: Instruction(STX,
                              3,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._stx),
            0x88: Instruction(DEY,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._dey),
            0x8A: Instruction(TXA,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._txa),
            0x8C: Instruction(STY,
                              4,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._sty),
            0x8D: Instruction(STA,
                              4,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._sta),
            0x8E: Instruction(STX,
                              4,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._stx),
            0x90: Instruction(BCC,
                              2,
                              AddressMode.RELATIVE,
                              self._get_relative_context,
                              False,
                              self._bcc),
            0x91: Instruction(STA,
                              6,
                              AddressMode.INDIRECT_Y,
                              self._get_indirect_y_context,
                              False,
                              self._sta),
            0x94: Instruction(STY,
                              4,
                              AddressMode.ZEROPAGE_X,
                              self._get_zeropage_x_context,
                              False,
                              self._sty),
            0x95: Instruction(STA,
                              4,
                              AddressMode.ZEROPAGE_X,
                              self._get_zeropage_x_context,
                              False,
                              self._sta),
            0x96: Instruction(STX,
                              4,
                              AddressMode.ZEROPAGE_Y,
                              self._resolve_zeropage_y_context,
                              False,
                              self._stx),
            0x98: Instruction(TYA,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._tya),
            0x99: Instruction(STA,
                              5,
                              AddressMode.ABSOLUTE_Y,
                              self._get_absolute_y_context,
                              False,
                              self._sta),
            0x9A: Instruction(TXS,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._txs),
            0x9D: Instruction(STA,
                              5,
                              AddressMode.ABSOLUTE_X,
                              self._get_absolute_x_context,
                              False,
                              self._sta),
            0xA0: Instruction(LDY,
                              2,
                              AddressMode.IMMEDIATE,
                              self._get_immediate_context,
                              False,
                              self._ldy),
            0xA1: Instruction(LDA,
                              6,
                              AddressMode.INDIRECT_X,
                              self._get_indirect_x_context,
                              False,
                              self._lda),
            0xA2: Instruction(LDX,
                              2,
                              AddressMode.IMMEDIATE,
                              self._get_immediate_context,
                              False,
                              self._ldx),
            0xA4: Instruction(LDY,
                              3,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._ldy),
            0xA5: Instruction(LDA,
                              3,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._lda),
            0xA6: Instruction(LDX,
                              3,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._ldx),
            0xA8: Instruction(TAY,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._tay),
            0xA9: Instruction(LDA,
                              2,
                              AddressMode.IMMEDIATE,
                              self._get_immediate_context,
                              False,
                              self._lda),
            0xAA: Instruction(TAX,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._tax),
            0xAC: Instruction(LDY,
                              4,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._ldy),
            0xAD: Instruction(LDA,
                              4,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._lda),
            0xAE: Instruction(LDX,
                              4,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._ldx),
            0xB0: Instruction(BCS,
                              2,
                              AddressMode.RELATIVE,
                              self._get_relative_context,
                              False,
                              self._bcs),
            0xB1: Instruction(LDA,
                              5,
                              AddressMode.INDIRECT_Y,
                              self._get_indirect_y_context,
                              True,
                              self._lda),
            0xB4: Instruction(LDY,
                              4,
                              AddressMode.ZEROPAGE_X,
                              self._get_zeropage_x_context,
                              False,
                              self._ldy),
            0xB5: Instruction(LDA,
                              4,
                              AddressMode.ZEROPAGE_X,
                              self._get_zeropage_x_context,
                              False,
                              self._lda),
            0xB6: Instruction(LDX,
                              4,
                              AddressMode.ZEROPAGE_Y,
                              self._resolve_zeropage_y_context,
                              False,
                              self._ldx),
            0xB8: Instruction(CLV,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._clv),
            0xB9: Instruction(LDA,
                              4,
                              AddressMode.ABSOLUTE_Y,
                              self._get_absolute_y_context,
                              True,
                              self._lda),
            0xBA: Instruction(TSX,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._tsx),
            0xBC: Instruction(LDY,
                              4,
                              AddressMode.ABSOLUTE_X,
                              self._get_absolute_x_context,
                              True,
                              self._ldy),
            0xBD: Instruction(LDA,
                              4,
                              AddressMode.ABSOLUTE_X,
                              self._get_absolute_x_context,
                              True,
                              self._lda),
            0xBE: Instruction(LDX,
                              4,
                              AddressMode.ABSOLUTE_Y,
                              self._get_absolute_y_context,
                              True,
                              self._ldx),
            0xC0: Instruction(CPY,
                              2,
                              AddressMode.IMMEDIATE,
                              self._get_immediate_context,
                              False,
                              self._cpy),
            0xC1: Instruction(CMP,
                              6,
                              AddressMode.INDIRECT_X,
                              self._get_indirect_x_context,
                              False,
                              self._cmp),
            0xC4: Instruction(CPY,
                              3,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._cpy),
            0xC5: Instruction(CMP,
                              3,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._cmp),
            0xC6: Instruction(DEC,
                              5,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._dec),
            0xC8: Instruction(INY,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._iny),
            0xC9: Instruction(CMP,
                              2,
                              AddressMode.IMMEDIATE,
                              self._get_immediate_context,
                              False,
                              self._cmp),
            0xCA: Instruction(DEX,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._dex),
            0xCC: Instruction(CPY,
                              4,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._cpy),
            0xCD: Instruction(CMP,
                              4,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._cmp),
            0xCE: Instruction(DEC,
                              6,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._dec),
            0xD0: Instruction(BNE,
                              2,
                              AddressMode.RELATIVE,
                              self._get_relative_context,
                              False,
                              self._bne),
            0xD1: Instruction(CMP,
                              5,
                              AddressMode.INDIRECT_Y,
                              self._get_indirect_y_context,
                              True,
                              self._cmp),
            0xD5: Instruction(CMP,
                              4,
                              AddressMode.ZEROPAGE_X,
                              self._get_zeropage_x_context,
                              False,
                              self._cmp),
            0xD6: Instruction(DEC,
                              6,
                              AddressMode.ZEROPAGE_X,
                              self._get_zeropage_x_context,
                              False,
                              self._dec),
            0xD8: Instruction(CLD,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._cld),
            0xD9: Instruction(CMP,
                              4,
                              AddressMode.ABSOLUTE_Y,
                              self._get_absolute_y_context,
                              True,
                              self._cmp),
            0xDD: Instruction(CMP,
                              4,
                              AddressMode.ABSOLUTE_X,
                              self._get_absolute_x_context,
                              True,
                              self._cmp),
            0xDE: Instruction(DEC,
                              7,
                              AddressMode.ABSOLUTE_X,
                              self._get_absolute_x_context,
                              False,
                              self._dec),
            0xE0: Instruction(CPX,
                              2,
                              AddressMode.IMMEDIATE,
                              self._get_immediate_context,
                              False,
                              self._cpx),
            0xE1: Instruction(SBC,
                              6,
                              AddressMode.INDIRECT_X,
                              self._get_indirect_x_context,
                              False,
                              self._sbc),
            0xE4: Instruction(CPX,
                              3,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._cpx),
            0xE5: Instruction(SBC,
                              3,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._sbc),
            0xE6: Instruction(INC,
                              5,
                              AddressMode.ZEROPAGE,
                              self._get_zeropage_context,
                              False,
                              self._inc),
            0xE8: Instruction(INX,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._inx),
            0xE9: Instruction(SBC,
                              2,
                              AddressMode.IMMEDIATE,
                              self._get_immediate_context,
                              False,
                              self._sbc),
            0xEA: Instruction(NOP,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._nop),
            0xEC: Instruction(CPX,
                              4,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._cpx),
            0xED: Instruction(SBC,
                              4,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._sbc),
            0xEE: Instruction(INC,
                              6,
                              AddressMode.ABSOLUTE,
                              self._get_absolute_context,
                              False,
                              self._inc),
            0xF0: Instruction(BEQ,
                              2,
                              AddressMode.RELATIVE,
                              self._get_relative_context,
                              False,
                              self._beq),
            0xF1: Instruction(SBC,
                              5,
                              AddressMode.INDIRECT_Y,
                              self._get_indirect_y_context,
                              True,
                              self._sbc),
            0xF5: Instruction(SBC,
                              4,
                              AddressMode.ZEROPAGE_X,
                              self._get_zeropage_x_context,
                              False,
                              self._sbc),
            0xF6: Instruction(INC,
                              6,
                              AddressMode.ZEROPAGE_X,
                              self._get_zeropage_x_context,
                              False,
                              self._inc),
            0xF8: Instruction(SED,
                              2,
                              AddressMode.IMPLIED,
                              self._get_implied_context,
                              False,
                              self._sed),
            0xF9: Instruction(SBC,
                              4,
                              AddressMode.ABSOLUTE_Y,
                              self._get_absolute_y_context,
                              True,
                              self._sbc),
            0xFD: Instruction(SBC,
                              4,
                              AddressMode.ABSOLUTE_X,
                              self._get_absolute_x_context,
                              True,
                              self._sbc),
            0xFE: Instruction(INC,
                              7,
                              AddressMode.ABSOLUTE_X,
                              self._get_absolute_x_context,
                              False,
                              self._inc)
        }
