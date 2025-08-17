from __future__ import annotations

from collections.abc import Generator, Sequence
from dataclasses import dataclass
from types import TracebackType
from typing import Any, Callable, Literal

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
type LoggingGen[A] = Generator[LoggingYield, LoggingSend, A]


def logable[**P, A](
    f: Callable[P, LoggingGen[A]],
) -> Callable[P, Logging[A]]:
    def _logging_internal(*args: P.args, **kwargs: P.kwargs) -> Logging[A]:
        return Logging(f(*args, **kwargs))

    return _logging_internal


@dataclass(frozen=True, slots=True)
class Logging[A]:
    decorated: LoggingGen[A]

    # generator protocol
    def send(self, value: LoggingSend) -> LoggingYield:
        return self.decorated.send(value)

    def throw(
        self, typ: type[BaseException] | BaseException, val: Any | None = None, tb: TracebackType | None = None
    ) -> LoggingYield:
        return self.decorated.throw(typ, val, tb)

    def close(self) -> None:
        return self.decorated.close()

    # iterator protocol
    def __next__(self) -> LoggingYield:
        return self.decorated.__next__()

    def __iter__(self) -> LoggingGen[A]:
        return self

    # Basic operation wrappers
    @logable
    @staticmethod
    def info(msg: str) -> LoggingGen[None]:
        yield LogMessage("info", msg)

    @logable
    @staticmethod
    def warning(msg: str) -> LoggingGen[None]:
        yield LogMessage("warning", msg)

    @logable
    @staticmethod
    def error(msg: str) -> LoggingGen[None]:
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


@logable
def add(x: int, y: int) -> LoggingGen[int]:
    yield from Logging.info(f"adding {x} and {y}")
    return x + y


@logable
def divide(x: int, y: int) -> LoggingGen[int]:
    yield from Logging.info(f"trying to divide {x} and {y}")
    if y == 0:
        yield from Logging.error(f"Can't divide {x} by 0, defaulting to -1")
        return -1
    return int(x / y)


@logable
def prog1() -> LoggingGen[int]:
    yield from Logging.info("Started prog1")
    a = 17
    b = 3
    c = yield from add(a, b)
    d = yield from add(2, 2)
    r = yield from divide(c, d)
    yield from Logging.info("Finished prog1")
    return r


@logable
def prog2() -> LoggingGen[int]:
    yield from Logging.info("Started prog2")
    a = 17
    b = 3
    c = yield from add(a, b)
    d = yield from add(2, -2)
    r = yield from divide(c, d)
    yield from Logging.info("Finished prog2")
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
