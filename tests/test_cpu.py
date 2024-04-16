import pathlib

import pytest

from nescpu.bus import CPUMemoryBus
from nescpu.cartridge import Cartridge
from nescpu.cpu import CPU, IllegalOpcodeError, TraceEntry

_BASE_DIR = pathlib.Path(__file__).parent
_ROM_DIR = _BASE_DIR / 'rom'
_TRACE_DIR = _BASE_DIR / 'cpu_traces'


def _is_hex_byte(value: str) -> bool:
    if len(value) != 2:
        return False
    try:
        int(value, 16)
    except ValueError:
        return False
    return True


def _parse_nestest_trace(file_path):
    trace = []
    with open(file_path, 'r') as file:
        for line in file:
            splited = line.split()
            pc = int(splited[0], 16)
            opcode = int(splited[1], 16)
            index = 2
            operand_bytes = []
            while _is_hex_byte(splited[index]):
                operand_bytes.append(int(splited[index], 16))
                index += 1
            if operand_bytes:
                operand = 0
                for idx, byte in enumerate(operand_bytes):
                    operand = operand | byte << 8 * idx
            else:
                operand = None
            mnemonic = splited[index]
            while not splited[index].startswith('A:'):
                index += 1
            a = int(splited[index].split(':')[1], 16)
            x = int(splited[index + 1].split(':')[1], 16)
            y = int(splited[index + 2].split(':')[1], 16)
            flags = int(splited[index + 3].split(':')[1], 16)
            sp = int(splited[index + 4].split(':')[1], 16)
            cycles = int(splited[-1].split(':')[1])
            trace_entry = TraceEntry(pc=pc,
                                     opcode=opcode,
                                     mnemonic=mnemonic,
                                     operand=operand,
                                     a=a,
                                     x=x,
                                     y=y,
                                     flags=flags,
                                     sp=sp,
                                     cycles=cycles)
            trace.append(trace_entry)
        return trace


@pytest.fixture(scope='session', params=[_TRACE_DIR / 'nestest.trace'])
def nestest_trace(request):
    return _parse_nestest_trace(request.param)


@pytest.fixture(params=[_ROM_DIR / 'nestest.nes'])
def cpu_nestest_trace(request):
    with open(request.param, 'rb') as rom:
        bus = CPUMemoryBus(Cartridge(rom.read()))
        cpu = CPU(bus)
        cpu.reset(pc=0xC000)
        trace = []
        while True:
            try:
                trace_entry = cpu.trace_tick()
            except IllegalOpcodeError:
                break
            trace.append(trace_entry)
        return trace


def test_cpu_nestest(cpu_nestest_trace, nestest_trace):
    for cpu_trace, nestest_trace in zip(cpu_nestest_trace, nestest_trace):
        assert cpu_trace == nestest_trace
