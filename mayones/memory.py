

class RAM:

    _SIZE = 2 * 1024

    def __init__(self):
        self._memory = bytearray(self._SIZE)

    def read(self, address: int) -> int:
        return self._memory[address]

    def write(self, address: int, data: int) -> None:
        self._memory[address] = data
        return None
