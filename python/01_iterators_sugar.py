from typing import Iterator


def range_iterator(start: int, end: int) -> Iterator[int]:
    assert start <= end, "range_iterator start must be smaller or equal to end"
    i = start
    while i < end:
        yield i
        i += 1


def main() -> None:
    for x in range_iterator(5, 8):
        print(f"{x=}")

    print("Done!")


if __name__ == "__main__":
    main()
