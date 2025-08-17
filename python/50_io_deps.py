from __future__ import annotations

import time
from collections.abc import Generator
from dataclasses import dataclass
from types import TracebackType
from typing import Any, Callable, Never, Sequence, override
from uuid import UUID, uuid4

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
    subtasks: Sequence[IO[Any, Any, Any]]


@dataclass(frozen=True, slots=True)
class RequestDependency[D]:
    dep_type: type[D]


# ----------------------------------------------------- #
# Messages from the interpreter back to the computation #
# ----------------------------------------------------- #


@dataclass(frozen=True, slots=True)
class ExitInPlace:
    error: Any


# ---------------- #
# They Either Type #
# ---------------- #

type IOYield[E, D] = StopFromError[E] | Gather | Sleep | RequestDependency[D]
type IOSend = Any | ExitInPlace
type IO[E, A, D] = Generator[IOYield[E, D], IOSend, A]

# ------------------------- #
# Basis operations wrappers #
# ------------------------- #


def halt_with_error[E](error: E) -> IO[E, Never, Never]:
    yield StopFromError(error)
    # NOTE: unreachable raise (based on interpreter impl, needed to avoid interpreter thinking we return a None)
    raise


def pure[A](value: A) -> IO[Never, A, Never]:
    return value
    # NOTE: unreachable yield is how to force the interpreter to make this function a genertor
    yield


# NOTE: Need many overloads to get type safe return value
def gather[E](sub_tasks: Sequence[IO[E, Any, Any]]) -> IO[E, Sequence[Any], Any]:
    r = yield Gather(sub_tasks)
    if isinstance(r, ExitInPlace):
        yield from halt_with_error(r.error)
        raise
    return r


def sleep(delay: float) -> IO[Never, None, Never]:
    yield Sleep(delay)


def get_dependency[D](dt: type[D]) -> IO[Never, D, D]:
    r = yield RequestDependency(dt)
    assert not isinstance(r, ExitInPlace), "getting dependencies should never fail"
    return r


# ----------- #
# Combinators #
# ----------- #


@dataclass(frozen=True)
class _RecoverWith[E, A, B, D]:
    decorated: IO[E, A, D]
    recover: Callable[[E], B]

    # generator protocol
    def send(self, value: IOSend) -> IOYield[Never, D]:
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
    def __next__(self) -> IOYield[Never, D]:
        return self.send(None)

    def __iter__(self) -> IO[Never, A | B, D]:
        return self


def recover_with[E, A, B, D](either: IO[E, A, D], recover: Callable[[E], B]) -> IO[Never, A | B, D]:
    return _RecoverWith(either, recover)


# ----------- #
# Interpreter #
# ----------- #


class DependencyResolver[D]:
    def resolve_dependendcy(self, d: type[D]) -> Any: ...


def run_io[E, A, D](exp: IO[E, A, D], dep_resolver: DependencyResolver[D]) -> A | StopFromError[E]:
    try:
        next_send: Any = None
        while True:
            command = exp.send(next_send)
            step_result = run_single_command(command, dep_resolver)
            if isinstance(step_result, StopFromError):
                exp.close()
                return step_result

            next_send = step_result
    except StopIteration as e:
        return e.value


def run_single_command[E, D](command: IOYield[E, D], dep_resolver: DependencyResolver) -> Any | StopFromError[E]:
    match command:
        case StopFromError() as stp:
            return stp
        case Sleep(delay):
            time.sleep(delay)
        case Gather(sub_commands):
            sub_results = [run_io(st, dep_resolver) for st in sub_commands]
            for sr in sub_results:
                if isinstance(sr, StopFromError):
                    # NOTE: instead of just exiting we need to send the error back to the parent copmutation, otherwise
                    #       the recover_with that is defined on the parent computation wont be triggered
                    return ExitInPlace(sr.error)
            return sub_results
        case RequestDependency(d):
            return dep_resolver.resolve_dependendcy(d)


# ------- #
# Example #
# ------- #


class Logger:
    def info(self, msg: str) -> None:
        print(f"INFO: {msg}")

    def warn(self, msg: str) -> None:
        print(f"WARNING: {msg}")

    def error(self, msg: str) -> None:
        print(f"ERROR: {msg}")


@dataclass(frozen=True, slots=True)
class EntryNotFound:
    id: UUID


@dataclass(frozen=True, slots=True)
class OutOfSpace:
    id: UUID


type ID = UUID


class StateDao:
    _registry: dict[UUID, int]

    def __init__(self) -> None:
        self._registry = {}

    def save_number(self, num: int) -> IO[OutOfSpace, UUID, Never]:
        uuid = uuid4()
        self._registry[uuid] = num
        return uuid
        # NOTE: unreachable yield is how to force the interpreter to make this function a genertor
        yield

    def get_number(self, uuid: UUID) -> IO[EntryNotFound, int, Never]:
        if uuid in self._registry:
            return self._registry[uuid]
        else:
            x = yield from halt_with_error(EntryNotFound(uuid))
            return x


def save_two_numbers(x: int, y: int) -> IO[OutOfSpace, tuple[UUID, UUID], StateDao]:
    state_dao = yield from get_dependency(StateDao)
    id1 = yield from state_dao.save_number(x)
    id2 = yield from state_dao.save_number(y)
    return id1, id2


def get_number_or_else(uuid: UUID, /, default: int) -> IO[Never, int, StateDao]:
    state_dao = yield from get_dependency(StateDao)
    r = yield from recover_with(state_dao.get_number(uuid), lambda e: default)
    return r


def prog1() -> IO[OutOfSpace, tuple[int, int], StateDao | Logger]:
    logger = yield from get_dependency(Logger)
    logger.info("Starting prog1")
    id1, _ = yield from save_two_numbers(7, 19)
    logger.info("Mid prog1")
    r1 = yield from get_number_or_else(id1, default=-1)
    r2 = yield from get_number_or_else(uuid4(), default=-1)
    logger.info("End prog1")
    return r1, r2


class SomeAdditionalDep: ...


def prog2() -> IO[Never, None, SomeAdditionalDep]:
    return None
    yield


class DependencyResolverImpl(DependencyResolver[Logger | StateDao]):
    _logger: Logger
    _state_dao: StateDao

    def __init__(self) -> None:
        self._logger = Logger()
        self._state_dao = StateDao()

    @override
    def resolve_dependendcy(self, d: type[Logger] | type[StateDao]) -> Any:
        if d == Logger:
            return self._logger
        elif d == StateDao:
            return self._state_dao
        else:
            raise AssertionError("Should not be here")


def main() -> None:
    r1 = run_io(prog1(), DependencyResolverImpl())

    print(f"{r1=}")

    # NOTE: this will not compile the dependency resolver doesn't match the type of prog2
    # r2 = run_io(prog2(), DependencyResolverImpl())


if __name__ == "__main__":
    main()
