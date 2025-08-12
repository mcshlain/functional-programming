from typing import Iterator


def range_iterator(start: int, end: int) -> Iterator[int]:
    assert start <= end, "range_iterator start must be smaller or equal to end"
    i = start
    while i < end:
        yield i
        i += 1


def list_iterator[A](lst: list[A]) -> Iterator[A]:
    for i in range_iterator(0, len(lst)):
        yield lst[i]


def combined_iterator() -> Iterator[int | str | float]:
    yield from range_iterator(3, 7)
    yield from list_iterator(["a", "b", "c"])
    yield 2.8


def main() -> None:
    for x in combined_iterator():
        print(f"{x=}")

    print("Done!")


if __name__ == "__main__":
    main()
