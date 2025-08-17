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
        return self.send(None)

    def __iter__(self) -> Either[Never, A | B]:
        return self


def recover_with[E, A, B](either: Either[E, A], recover: Callable[[E], B]) -> Either[Never, A | B]:
    return _RecoverWith(either, recover)


@dataclass
class _RecoverWithEither[E1, E2, A, B]:
    decorated: Either[E1, A]
    recover: Callable[[E1], Either[E2, B]]
    decorated_recover: Either[E2, B] | None = None

    # generator protocol
    def send(self, value: EitherSend) -> EitherYield[E2]:
        if self.decorated_recover is not None:
            return self.decorated_recover.send(value)
        else:
            v = self.decorated.send(value)

            if isinstance(v, StopFromError):
                self.decorated_recover = self.recover(v.error)
                return next(self.decorated_recover)  # start the recover io
            else:
                pass

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
    def __next__(self) -> EitherYield[E2]:
        return self.send(None)

    def __iter__(self) -> Either[E2, A | B]:
        return self


def recover_with_either[E1, E2, A, B](
    io: Either[E1, A], recover_io: Callable[[E1], Either[E2, B]]
) -> Either[E2, A | B]:
    return _RecoverWithEither(io, recover_io)


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


@dataclass(frozen=True, slots=True)
class DivideByZero:
    msg: str


@dataclass(frozen=True, slots=True)
class InternalError:
    msg: str


def divide(a: int, b: int) -> Either[DivideByZero, int]:
    if b == 0:
        x = yield from halt_with_error(DivideByZero(f"can't divide {a} by 0"))
        return x
    else:
        return int(a / b)


def prog1() -> Either[DivideByZero, int]:
    a = yield from pure(6)
    b = yield from pure(3)
    r = yield from divide(a, b)
    return r


def prog2() -> Either[DivideByZero | InternalError, int]:
    a = yield from pure(6)
    b = yield from halt_with_error(InternalError("prog2 mb intentional error"))
    r = yield from divide(a, b)
    return r


def prog3() -> Either[DivideByZero, int]:
    a = yield from pure(6)
    b = yield from pure(0)
    r = yield from divide(a, b)
    return r


def prog4() -> Either[Never, int | str]:
    r = yield from recover_with(divide(6, 0), lambda _: "oops")
    return r


def _recover_from_internal_error_only[E](e: E | InternalError) -> Either[E, int]:
    if isinstance(e, InternalError):
        return 0
    else:
        x = yield from halt_with_error(e)
        return x


def main() -> None:
    r1 = run_either(prog1())
    r2 = run_either(prog2())
    r3 = run_either(prog3())

    print(f"{r1=}\n{r2=}\n{r3=}")

    r4 = run_either(prog4())
    print(f"{r4=}")

    r5 = run_either(recover_with_either(prog2(), _recover_from_internal_error_only))
    r6 = run_either(recover_with_either(prog3(), _recover_from_internal_error_only))
    print(f"{r5=} {r6=}")


if __name__ == "__main__":
    main()
