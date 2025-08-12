from typing import Iterator


class RangeIterator:
    _current: int
    _end: int

    def __init__(self, start: int, end: int) -> None:
        assert start <= end, "RangeIterator start must be smaller or equal to end"
        self._current = start
        self._end = end

    def __next__(self) -> int:
        if self._current == self._end:
            raise StopIteration()
        value_to_yield = self._current
        self._current += 1
        return value_to_yield

    def __iter__(self) -> Iterator[int]:
        return self


def main() -> None:
    iterator = RangeIterator(5, 8)

    try:
        while True:
            x = next(iterator)
            print(f"{x=}")
    except StopIteration:
        pass

    print("Done!")


if __name__ == "__main__":
    main()
