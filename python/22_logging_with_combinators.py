from __future__ import annotations

from abc import ABC, abstractmethod
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


@dataclass(frozen=True, slots=True)
class Nop: ...


# ---------------- #
# The Logging Type #
# ---------------- #

type LoggingYield = LogMessage | Nop
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

    # Combinators
    def muted(self) -> Logging[A]:
        return Logging(_MuteLogging(self))

    def errors_as_warning(self) -> Logging[A]:
        return Logging(_TransformSeverity(self, "error", "warning"))


# ----------- #
# Combinators #
# ----------- #


@dataclass(frozen=True, slots=True)
class _CombinatorBase[A](ABC):
    decorated: LoggingGen[A]

    # generator protocol
    @abstractmethod
    def send(self, value: LoggingSend) -> LoggingYield: ...

    def throw(
        self, typ: type[BaseException] | BaseException, val: Any | None = None, tb: TracebackType | None = None
    ) -> LoggingYield:
        return self.decorated.throw(typ, val, tb)

    def close(self) -> None:
        return self.decorated.close()

    # iterator protocol
    def __next__(self) -> LoggingYield:
        return self.send(None)

    def __iter__(self) -> LoggingGen[A]:
        return self


@dataclass(frozen=True, slots=True)
class _MuteLogging[A](_CombinatorBase[A]):
    def send(self, value: LoggingSend) -> LoggingYield:
        v = self.decorated.send(value)
        if isinstance(v, LogMessage):
            return Nop()
        return v


@dataclass(frozen=True, slots=True)
class _TransformSeverity[A](_CombinatorBase[A]):
    decorated: LoggingGen[A]
    from_severity: Severity
    to_severity: Severity

    # generator protocol
    def send(self, value: LoggingSend) -> LoggingYield:
        v = self.decorated.send(value)
        if isinstance(v, LogMessage) and v.severity == self.from_severity:
            return LogMessage(self.to_severity, v.message)
        return v


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
    c = yield from add(a, b).muted()
    d = yield from add(2, -2).muted()
    r = yield from divide(c, d).errors_as_warning()
    yield from Logging.info("Finished prog1")
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


if __name__ == "__main__":
    main()
