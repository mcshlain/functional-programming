from __future__ import annotations

from collections.abc import Generator
from dataclasses import dataclass
from types import TracebackType
from typing import Any, Callable, Never


@dataclass(frozen=True, slots=True)
class StopFromError[E]:
    error: E


type EitherYield[E] = StopFromError[E]
type EitherSend = Any
type EitherGen[E, A] = Generator[EitherYield[E], EitherSend, A]


@dataclass(frozen=True, slots=True)
class Either[E, A]:
    decorated: EitherGen[E, A]

    # generator protocol
    def send(self, value: EitherSend) -> EitherYield[E]:
        return self.decorated.send(value)

    def throw(
        self, typ: type[BaseException] | BaseException, val: Any | None = None, tb: TracebackType | None = None
    ) -> EitherYield[E]:
        return self.decorated.throw(typ, val, tb)

    def close(self) -> None:
        return self.decorated.close()

    # iterator protocol
    def __next__(self) -> EitherYield[E]:
        return self.decorated.__next__()

    def __iter__(self) -> EitherGen[E, A]:
        return self

    # basic operations
    @staticmethod
    def error[E2](error: E2) -> Either[E2, Never]:
        return _error(error)

    @staticmethod
    def pure[A2](value: A2) -> Either[Never, A2]:
        return _pure(value)

    # Combinators
    def recover_with[B](self, f: Callable[[E], B]) -> Either[Never, A | B]:
        return Either(_RecoverWith(f, self))


def eitherable[**P, E, A](
    f: Callable[P, EitherGen[E, A]],
) -> Callable[P, Either[E, A]]:
    def _eitherable_internal(*args: P.args, **kwargs: P.kwargs) -> Either[E, A]:
        return Either(f(*args, **kwargs))

    return _eitherable_internal


@eitherable
def _error[E](error: E) -> EitherGen[E, Never]:
    yield StopFromError(error)
    # NOTE: unreachable raise (based on interpreter impl, needed to avoid interpreter thinking we return a None)
    raise


@eitherable
def _pure[A](value: A) -> EitherGen[Never, A]:
    return value
    # NOTE: unreachable yield is how to force the interpreter to make this function a genertor
    yield


@dataclass(frozen=True)
class _RecoverWith[E, A, B]:
    recover: Callable[[E], B]
    decorated: EitherGen[E, A]

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

    def __iter__(self) -> EitherGen[Never, A | B]:
        return self


# ----------- #
# Interpreter #
# ----------- #


def run_either[E, A](exp: Either[E, A]) -> A | StopFromError[E]:
    try:
        next_send: Any = None
        while True:
            maybe = exp.send(next_send)
            match maybe:
                case StopFromError() as stp:
                    exp.close()
                    return stp
    except StopIteration as e:
        return e.value


# ------- #
# Example #
# ------- #


@eitherable
def divide(a: int, b: int) -> EitherGen[str, int]:
    if b == 0:
        x = yield from Either.error(f"can't divide {a} by 0")
        return x
    else:
        return int(a / b)


@eitherable
def prog1() -> EitherGen[str, int]:
    a = yield from Either.pure(6)
    b = yield from Either.pure(3)
    r = yield from divide(a, b)
    return r


@eitherable
def prog2() -> EitherGen[str, int]:
    a = yield from Either.pure(6)
    b = yield from Either.error("prog2 mb intentional error")
    r = yield from divide(a, b)
    return r


@eitherable
def prog3() -> EitherGen[str, int]:
    a = yield from Either.pure(6)
    b = yield from Either.pure(0)
    r = yield from divide(a, b)
    return r


@eitherable
def prog4() -> EitherGen[Never, int | str]:
    r = yield from divide(6, 0).recover_with(lambda _: "oops")
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
