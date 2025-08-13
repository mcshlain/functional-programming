from __future__ import annotations

import asyncio
from collections.abc import Generator
from dataclasses import dataclass
from types import TracebackType
from typing import Any, Callable, Never, Sequence

# -------------- #
# Syntax as Data #
# -------------- #


@dataclass(frozen=True, slots=True)
class StopFromError[E]:
    error: E


@dataclass(frozen=True, slots=True)
class Sleep:
    delay: float


@dataclass(frozen=True, slots=True)
class Gather:
    subtasks: Sequence[IO[Any, Any]]


# ----------------------------------------------------- #
# Messages from the interpreter back to the computation #
# ----------------------------------------------------- #


@dataclass(frozen=True, slots=True)
class ExitInPlace:
    error: Any


# ---------------- #
# They Either Type #
# ---------------- #

type IOYield[E] = StopFromError[E] | Gather | Sleep
type IOSend = Any | ExitInPlace
type IO[E, A] = Generator[IOYield[E], IOSend, A]

# ------------------------- #
# Basis operations wrappers #
# ------------------------- #


def halt_with_error[E](error: E) -> IO[E, Never]:
    yield StopFromError(error)
    # NOTE: unreachable raise (based on interpreter impl, needed to avoid interpreter thinking we return a None)
    raise


def pure[A](value: A) -> IO[Never, A]:
    return value
    # NOTE: unreachable yield is how to force the interpreter to make this function a genertor
    yield


# NOTE: Need many overloads to get type safe return value
def gather[E](sub_tasks: Sequence[IO[E, Any]]) -> IO[E, Sequence[Any]]:
    r = yield Gather(sub_tasks)
    if isinstance(r, ExitInPlace):
        yield from halt_with_error(r.error)
        raise
    return r


def sleep(delay: float) -> IO[Never, None]:
    yield Sleep(delay)


# ----------- #
# Combinators #
# ----------- #


@dataclass(frozen=True)
class _RecoverWith[E, A, B]:
    decorated: IO[E, A]
    recover: Callable[[E], B]

    # generator protocol
    def send(self, value: IOSend) -> IOYield[Never]:
        v = self.decorated.send(value)
        if isinstance(v, StopFromError):
            e = StopIteration()
            e.value = self.recover(v.error)
            raise e
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
    def __next__(self) -> IOYield[Never]:
        v = self.decorated.__next__()
        if isinstance(v, StopFromError):
            e = StopIteration()
            e.value = self.recover(v.error)
            raise e
        return v

    def __iter__(self) -> IO[Never, A | B]:
        return self


def recover_with[E, A, B](either: IO[E, A], recover: Callable[[E], B]) -> IO[Never, A | B]:
    return _RecoverWith(either, recover)


# ----------- #
# Interpreter #
# ----------- #


async def run_io[E, A](exp: IO[E, A]) -> A | StopFromError[E]:
    try:
        next_send: Any = None
        while True:
            command = exp.send(next_send)
            step_result = await run_single_command(command)
            if isinstance(step_result, StopFromError):
                exp.close()
                return step_result

            next_send = step_result
    except StopIteration as e:
        return e.value


async def run_single_command[E](command: IOYield[E]) -> Any | StopFromError[E]:
    match command:
        case StopFromError() as stp:
            return stp
        case Sleep(delay):
            await asyncio.sleep(delay)
        case Gather(sub_tasks):
            sub_results = await asyncio.gather(*[run_io(st) for st in sub_tasks])
            for sr in sub_results:
                if isinstance(sr, StopFromError):
                    # NOTE: instead of just exiting we need to send the error back to the parent copmutation, otherwise
                    #       the recover_with that is defined on the parent computation wont be triggered
                    return ExitInPlace(sr.error)
            return sub_results


# ------- #
# Example #
# ------- #


@dataclass(frozen=True, slots=True)
class PrefixToLong:
    pass


def prefix_of(s: str, /, length: int) -> IO[PrefixToLong, str]:
    if len(s) < length:
        x = yield from halt_with_error(PrefixToLong())
        return x
    else:
        # NOTE: simulate long computation
        yield from sleep(1)
        return s[0:length]


@dataclass(frozen=True, slots=True)
class SuffixToLong:
    pass


def suffix_of(s: str, /, length: int) -> IO[SuffixToLong, str]:
    if len(s) < length:
        x = yield from halt_with_error(SuffixToLong())
        return x
    else:
        # NOTE: simulate long computation
        yield from sleep(0.5)
        return s[len(s) - length :]


def prog1() -> IO[PrefixToLong | SuffixToLong, int]:
    st1 = prefix_of("abcdefgh", 4)
    st2 = suffix_of("abcdefgh", 2)

    r1, r2 = yield from gather([st1, st2])
    return r1 + r2


def prog2() -> IO[PrefixToLong | SuffixToLong, int]:
    st1 = prefix_of("abcdefgh", 4)
    st2 = suffix_of("abcdefgh", 20)

    r1, r2 = yield from gather([st1, st2])
    return r1 + r2


async def main() -> None:
    r1 = await run_io(prog1())

    print(f"{r1=}")

    r2 = await run_io(prog2())

    print(f"{r2=}")

    r3 = await run_io(
        recover_with(prog2(), lambda e: "suffix error" if isinstance(e, SuffixToLong) else "prefix error")
    )

    print(f"{r3=}")


if __name__ == "__main__":
    asyncio.run(main())
