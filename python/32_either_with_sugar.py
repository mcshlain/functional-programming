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

    def recover_with_either[E2, B](self, recover_either: Callable[[E], Either[E2, B]]) -> Either[E2, A | B]:
        return Either(_RecoverWithEither(self, recover_either))


def eitherable[**P, E, A](
    f: Callable[P, EitherGen[E, A]],
) -> Callable[P, Either[E, A]]:
    def _eitherable_internal(*args: P.args, **kwargs: P.kwargs) -> Either[E, A]:
        return Either(f(*args, **kwargs))

    return _eitherable_internal


# ----------------------------------- #
# Basis operations wrappers (private) #
# ----------------------------------- #


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


# ----------- #
# Combinators #
# ----------- #


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
        return self.send(None)

    def __iter__(self) -> EitherGen[Never, A | B]:
        return self


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

    def __iter__(self) -> EitherGen[E2, A | B]:
        return self


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


@dataclass(frozen=True, slots=True)
class Oops:
    msg: str


@eitherable
def divide(a: int, b: int) -> EitherGen[DivideByZero, int]:
    if b == 0:
        x = yield from Either.error(DivideByZero(f"can't divide {a} by 0"))
        return x
    else:
        return int(a / b)


@eitherable
def prog1() -> EitherGen[DivideByZero, int]:
    a = yield from Either.pure(6)
    b = yield from Either.pure(3)
    r = yield from divide(a, b)
    return r


@eitherable
def prog2() -> EitherGen[DivideByZero | InternalError, int]:
    a = yield from Either.pure(6)
    b = yield from Either.error(InternalError("prog2 mb intentional error"))
    r = yield from divide(a, b)
    return r


@eitherable
def prog3() -> EitherGen[DivideByZero, int]:
    a = yield from Either.pure(6)
    b = yield from Either.pure(0)
    r = yield from divide(a, b)
    return r


@eitherable
def prog4() -> EitherGen[Never, int | str]:
    r = yield from divide(6, 0).recover_with(lambda _: "oops")
    return r


@eitherable
def _recover_from_internal_error_only[E](e: E | InternalError) -> EitherGen[E, int]:
    if isinstance(e, InternalError):
        return 0
    else:
        x = yield from Either.error(e)
        return x


def main() -> None:
    r1 = run_either(prog1())
    r2 = run_either(prog2())
    r3 = run_either(prog3())

    print(f"{r1=}\n{r2=}\n{r3=}")

    r4 = run_either(prog4())
    print(f"{r4=}")

    r5 = run_either(prog2().recover_with_either(_recover_from_internal_error_only))
    r6 = run_either(prog3().recover_with_either(_recover_from_internal_error_only))
    print(f"{r5=} {r6=}")


if __name__ == "__main__":
    main()
