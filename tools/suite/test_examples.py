from suite.support import (
    oracle_run,
    read,
    repo,
    run,
    runnable_examples,
)

# Every runnable example, compiled by the compiler written in Moss and
# run, prints its golden output; and so does the oracle.


def test_examples_match_their_goldens():
    for name in runnable_examples():
        golden = read(f"tests/examples/stdout/{name}.txt")
        assert run(f"examples/{name}.moss", []) == golden, f"{name}"


# The oracle runs each example too: the Rocq front end, elaborator and
# interpreter agree with the golden (and so with the compiler).


def test_the_oracle_matches_the_goldens():
    for name in runnable_examples():
        golden = read(f"tests/examples/stdout/{name}.txt")
        assert oracle_run(f"examples/{name}.moss", []) == golden, f"{name}"


# The other direction of the listing: a golden once outlived its example
# by a month because nothing looked.


def test_every_golden_belongs_to_an_example():

    goldens = {path.stem for path in (repo() / "tests/examples/stdout").glob("*.txt")}
    assert goldens == set(runnable_examples())
