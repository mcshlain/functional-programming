from typing import Generator, Literal

type PongGen = Generator[Literal["pong"], Literal["ping"], Literal["done"]]


# NOTE: generators are annoying to write without the sytactic sugar so we avoid it
def pong(amount: int) -> PongGen:
    for _ in range(amount):
        # NOTE: with generators yield syntax the yield now returns a value!
        recieved_from_ping = yield "pong"
        print(f"pong got: {recieved_from_ping}")

    return "done"


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
    ping(pong(3))


if __name__ == "__main__":
    main()
