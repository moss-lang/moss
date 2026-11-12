"""Corpus workers visit every input and report results in input order."""

from threading import Lock

from suite.support import parallel_map


def test_every_input_is_checked_once_and_results_keep_their_order():
    for length in (0, 1, 3, 37):
        inputs = list(range(length))
        for workers in (1, 4, 64):
            visits = [0] * length
            lock = Lock()

            def check(index):
                with lock:
                    visits[index] += 1
                return index * index

            assert parallel_map(inputs, check, workers) == [index * index for index in inputs]
            assert visits == [1] * length


def test_a_worker_failure_is_not_lost():
    def check(value):
        assert value != 1, "a corpus failure must fail the test"

    try:
        parallel_map([0, 1, 2], check, 2)
    except AssertionError as error:
        assert "a corpus failure must fail the test" in str(error)
    else:
        raise AssertionError("the worker failure did not reach the caller")
