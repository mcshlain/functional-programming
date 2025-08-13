from __future__ import annotations

from collections.abc import Generator
from dataclasses import dataclass
from types import TracebackType
from typing import Any, Callable, Never

# -------------- #
# Syntax as Data #
# -------------- #


@dataclass(frozen=True, slots=True)
class StopFromError[E]:
    error: E


# --------------- #
# The Either Type #
# --------------- #

type EitherYield[E] = StopFromError[E]
type EitherSend = Any
type Either[E, A] = Generator[EitherYield[E], EitherSend, A]

# ------------------------- #
# Basis operations wrappers #
# ------------------------- #


def halt_with_error[E](error: E) -> Either[E, Never]:
    yield StopFromError(error)
    # NOTE: unreachable raise (based on interpreter impl, needed to avoid interpreter thinking we return a None)
    raise


def pure[A](value: A) -> Either[Never, A]:
    return value
    # NOTE: unreachable yield is how to force the interpreter to make this function a genertor
    yield


# ----------- #
# Combinators #
# ----------- #


@dataclass(frozen=True)
class _RecoverWith[E, A, B]:
    decorated: Either[E, A]
    recover: Callable[[E], B]

    # generator protocol
    def send(self, value: EitherSend) -> EitherYield[Never]:
        v = self.decorated.send(value)
        if isinstance(v, StopFromError):
            e = StopIteration()
            e.value = self.recover(v.error)
            raise e
        return v

    def throw(
        self,
        typ: type[BaseException] | BaseException,
        val: Any | None = None,
        tb: TracebackType | None = None,
    ) -> Any:
        return self.decorated.throw(typ, val, tb)

    def close(self) -> None:
        self.decorated.close()

    # iterator protocol
    def __next__(self) -> EitherYield[Never]:
        v = self.decorated.__next__()
        if isinstance(v, StopFromError):
            e = StopIteration()
            e.value = self.recover(v.error)
            raise e
        return v

    def __iter__(self) -> Either[Never, A | B]:
        return self


def recover_with[E, A, B](either: Either[E, A], recover: Callable[[E], B]) -> Either[Never, A | B]:
    return _RecoverWith(either, recover)


# ----------- #
# Interpreter #
# ----------- #


def run_either[E, A](exp: Either[E, A]) -> A | StopFromError[E]:
    try:
        next_send: Any = None
        while True:
            command = exp.send(next_send)
            match command:
                case StopFromError() as stp:
                    exp.close()
                    return stp
    except StopIteration as e:
        return e.value


# ------- #
# Example #
# ------- #


def divide(a: int, b: int) -> Either[str, int]:
    if b == 0:
        x = yield from halt_with_error(f"can't divide {a} by 0")
        return x
    else:
        return int(a / b)


def prog1() -> Either[str, int]:
    a = yield from pure(6)
    b = yield from pure(3)
    r = yield from divide(a, b)
    return r


def prog2() -> Either[str, int]:
    a = yield from pure(6)
    b = yield from halt_with_error("prog2 mb intentional error")
    r = yield from divide(a, b)
    return r


def prog3() -> Either[str, int]:
    a = yield from pure(6)
    b = yield from pure(0)
    r = yield from divide(a, b)
    return r


def prog4() -> Either[Never, int | str]:
    r = yield from recover_with(divide(6, 0), lambda _: "oops")
    return r


def main() -> None:
    r1 = run_either(prog1())
    r2 = run_either(prog2())
    r3 = run_either(prog3())

    print(f"{r1=}\n{r2=}\n{r3=}")

    r4 = run_either(prog4())
    print(f"{r4=}")


if __name__ == "__main__":
    main()
