from __future__ import annotations

from collections.abc import Generator
from dataclasses import dataclass
from typing import Any, Protocol

# -------------- #
# Syntax as Data #
# -------------- #


@dataclass(frozen=True, slots=True)
class GetState:
    state_type: type[Any]


@dataclass(frozen=True, slots=True)
class SetState:
    state: Any


# -------------- #
# The State Type #
# -------------- #


class Updatable(Protocol):
    def update(self, other: Updatable) -> None: ...


type StateYield = SetState | GetState
type StateSend[S: Updatable] = S | None
type State[S: Updatable, A] = Generator[StateYield, StateSend[S], A]


# --------------- #
# Basic Operation #
# --------------- #


def set_state[S: Updatable](state: S) -> State[S, None]:
    yield SetState(state)


def get_state[S: Updatable](state_type: type[S]) -> State[S, S]:
    r = yield GetState(state_type)
    assert r is not None
    return r


# ----------- #
# Interpreter #
# ----------- #


def run_state[S: Updatable, A](et: State[S, A], initial_state: S) -> tuple[A, S]:
    internal_state: S = initial_state
    try:
        next_send: S | None = None
        while True:
            s = et.send(next_send)
            match s:
                case GetState():
                    next_send = internal_state
                case SetState(value):
                    internal_state.update(value)
                    next_send = None
    except StopIteration as e:
        return e.value, internal_state


# ------- #
# Example #
# ------- #


@dataclass
class ContainerA:
    value_int: int

    def update(self, other: Updatable) -> None:
        if isinstance(other, ContainerA):
            self.value_int = other.value_int


@dataclass
class ContainerB:
    value_str: str

    def update(self, other: Updatable) -> None:
        if isinstance(other, ContainerB):
            self.value_str = other.value_str


def prog1() -> State[ContainerA, int]:
    x = 1
    y = 2
    yield from set_state(ContainerA(1))
    return x + y


def prog2() -> State[ContainerB, int]:
    x = 7
    y = 8
    yield from set_state(ContainerB("hello"))
    return x + y


@dataclass
class ContainerAB(ContainerA, ContainerB):
    def update(self, other: Updatable) -> None:
        if isinstance(other, ContainerA):
            ContainerA.update(self, other)
        if isinstance(other, ContainerB):
            ContainerB.update(self, other)


def prog3() -> State[ContainerAB, ContainerA]:
    yield from prog1()
    yield from prog2()

    ca = yield from get_state(ContainerA)

    return ca


def main() -> None:
    r1 = run_state(prog1(), ContainerA(0))
    print(f"{r1=}")
    r2 = run_state(prog2(), ContainerB("no"))
    print(f"{r2=}")
    r3 = run_state(prog3(), ContainerAB("no", 0))
    print(f"{r3=}")


if __name__ == "__main__":
    main()
