from __future__ import annotations

import random
from dataclasses import dataclass
from typing import Generator, Literal, Sequence


@dataclass(frozen=True, slots=True)
class BinaryTree[A]:
    value: A
    left: BinaryTree[A] | None = None
    right: BinaryTree[A] | None = None


type WalkDirection = Literal["left", "right"]
type TreeWalkGen[A] = Generator[A, WalkDirection, None]


def binary_tree_walk[A](tree: BinaryTree[A]) -> TreeWalkGen[A]:
    current: BinaryTree[A] | None = tree
    while current is not None:
        where_to_next = yield current.value
        if where_to_next == "left":
            current = current.left
        else:  # where_to_next == "right"
            current = current.right


def left_only_path_driver[A](walk_gen: TreeWalkGen[A]) -> Sequence[A]:
    collected_list: list[A] = []
    try:
        # start the generator until the first value is produced (the root)
        first_value = next(walk_gen)
        collected_list.append(first_value)
        while True:
            # send the direction to keep going left
            other_value = walk_gen.send("left")
            collected_list.append(other_value)
    except StopIteration:
        pass

    return collected_list


def random_path_driver[A](
    walk_gen: TreeWalkGen[A],
) -> Sequence[tuple[A, WalkDirection]]:
    collected_list: list[A] = []
    chosen_directions: list[WalkDirection] = []
    try:
        # start the generator until the first value is produced (the root)
        first_value = next(walk_gen)
        collected_list.append(first_value)
        while True:
            # choose a random direction
            chosen_direction: WalkDirection = random.choice(["left", "right"])
            chosen_directions.append(chosen_direction)

            # send the next step back to the generator
            other_value = walk_gen.send(chosen_direction)
            collected_list.append(other_value)
    except StopIteration:
        pass

    return list(zip(collected_list, chosen_directions))


tree_str = """
       (0)
       / \\
      /   \\
    (1)   (4)
    /     / \\
   /     /   \\
  (2)   (5)  (6)
   \\
   (3)
"""


def main() -> None:
    leaf6 = BinaryTree(6)
    leaf5 = BinaryTree(5)
    node4 = BinaryTree(4, left=leaf5, right=leaf6)
    leaf3 = BinaryTree(3)
    node2 = BinaryTree(2, right=leaf3)
    node1 = BinaryTree(1, left=node2)
    root = BinaryTree(0, left=node1, right=node4)

    print(tree_str)

    left_most_path = left_only_path_driver(binary_tree_walk(root))
    print(f"{left_most_path=}")

    random_path1 = random_path_driver(binary_tree_walk(root))
    print(f"{random_path1=}")

    random_path2 = random_path_driver(binary_tree_walk(root))
    print(f"{random_path2=}")


if __name__ == "__main__":
    main()
