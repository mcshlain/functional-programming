from __future__ import annotations

from collections.abc import Generator, Sequence
from dataclasses import dataclass
from typing import Literal

# -------------- #
# Syntax as Data #
# -------------- #


type Severity = Literal["error", "warning", "info"]


@dataclass(frozen=True, slots=True)
class LogMessage:
    severity: Severity
    message: str


# ---------------- #
# The Logging Type #
# ---------------- #

type LoggingYield = LogMessage
type LoggingSend = None
type Logging[A] = Generator[LoggingYield, LoggingSend, A]

# ------------------------- #
# Basis operations wrappers #
# ------------------------- #


def log_info(msg: str) -> Logging[None]:
    yield LogMessage("info", msg)


def log_warning(msg: str) -> Logging[None]:
    yield LogMessage("warning", msg)


def log_error(msg: str) -> Logging[None]:
    yield LogMessage("error", msg)


# ------------ #
# Interpreters #
# ------------ #


def run_printing[A](exp: Logging[A]) -> A:
    try:
        while True:
            command = exp.send(None)
            match command:
                case LogMessage(severity, msg):
                    print(f"{severity.upper()}: {msg}")
    except StopIteration as e:
        return e.value


def run_muted[A](exp: Logging[A]) -> A:
    try:
        while True:
            command = exp.send(None)
            match command:
                case LogMessage():
                    pass  # Do nothing
    except StopIteration as e:
        return e.value


def run_collecting[A](exp: Logging[A]) -> tuple[A, Sequence[LogMessage]]:
    log_messages: list[LogMessage] = []
    try:
        while True:
            command = exp.send(None)
            match command:
                case LogMessage() as log:
                    log_messages.append(log)
    except StopIteration as e:
        return e.value, log_messages


# ------- #
# Example #
# ------- #


def add(x: int, y: int) -> Logging[int]:
    yield from log_info(f"adding {x} and {y}")
    return x + y


def divide(x: int, y: int) -> Logging[int]:
    yield from log_info(f"trying to divide {x} and {y}")
    if y == 0:
        yield from log_error(f"Can't divide {x} by 0, defaulting to -1")
        return -1
    return int(x / y)


def prog1() -> Logging[int]:
    yield from log_info("Started prog1")
    a = 17
    b = 3
    c = yield from add(a, b)
    d = yield from add(2, 2)
    r = yield from divide(c, d)
    yield from log_info("Finished prog1")
    return r


def prog2() -> Logging[int]:
    yield from log_info("Started prog2")
    a = 17
    b = 3
    c = yield from add(a, b)
    d = yield from add(2, -2)
    r = yield from divide(c, d)
    yield from log_info("Finished prog2")
    return r


def main() -> None:
    print("prog1 before run_printing")
    r1 = run_printing(prog1())
    print(f"{r1=}")

    print("prog1 before run_muted")
    r2 = run_muted(prog1())
    print(f"{r2=}")

    print("prog1 before run_collecting")
    r3 = run_collecting(prog1())
    print(f"{r3=}")

    print("----------")

    print("prog2 before run_printing")
    r4 = run_printing(prog2())
    print(f"{r4=}")

    print("prog2 before run_muted")
    r5 = run_muted(prog2())
    print(f"{r5=}")

    print("prog2 before run_collecting")
    r6 = run_collecting(prog2())
    print(f"{r6=}")


if __name__ == "__main__":
    main()
