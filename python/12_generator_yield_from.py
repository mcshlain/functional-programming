from typing import Generator, Literal

type PongGen = Generator[Literal["pong"], Literal["ping"], Literal["done"]]


def pong(amount: int) -> PongGen:
    for _ in range(amount):
        # NOTE: with generators yield syntax the yield now returns a value!
        recieved_from_ping = yield "pong"
        print(f"pong got: {recieved_from_ping}")

    return "done"


# NOTE: middleman_X don't interact with the communication between ping and pong, only the return value
# NOTE: we can have an arbitrary number of such middleman, as long as they only use 'yield from'
def middleman_1(amount: int) -> PongGen:
    result = yield from pong(amount)
    return result


def middleman_2(amount: int) -> PongGen:
    result = yield from middleman_1(amount)
    return result


def ping(gen: PongGen) -> None:
    try:
        recieved_from_pong = next(gen)  # Can also do gen.send(None)
        print(f"ping got: {recieved_from_pong}")
        while True:
            recieved_from_pong = gen.send("ping")  # Can also do gen.send(None)
            print(f"ping got: {recieved_from_pong}")

    # NOTE: like iterators when generator is done it throws StopIteration
    #       but now it also carries the return type
    except StopIteration as e:
        print(f"ping got return value of: {e.value}")


def main() -> None:
    ping(middleman_2(3))


if __name__ == "__main__":
    main()
