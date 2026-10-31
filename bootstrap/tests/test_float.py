import math
import struct
import unittest

from mossc import interp
from mossc.interp import MossPanic, f32, fdiv, fmax, fmin, fround, fsqrt, int_to_float
from mossc.interp import reinterpret, trunc_to_int

from .test_run import FLOAT_OUT, NEWTON_OUT, run


def bits32(x: float) -> int:
    return struct.unpack("<I", struct.pack("<f", x))[0]


class TestFloatSemantics(unittest.TestCase):
    """The interpreter's float instructions, against what Wasm specifies
    (D67): an f32 is rounded to single after every operation, a NaN is
    unordered, and the conversions round or trap as their names say."""

    def test_f32_rounds_to_single_precision(self):
        self.assertEqual(f32(1 / 3), 0.3333333432674408)
        self.assertEqual(bits32(f32(1 / 3)), 0x3EAAAAAB)
        self.assertNotEqual(f32(0.1), 0.1)
        # Exactly halfway between two singles: ties to even.
        self.assertEqual(f32(1 + 2**-24), 1.0)
        self.assertEqual(f32(1 + 3 * 2**-24), 1 + 2**-22)

    def test_f32_overflow_is_infinite_not_an_error(self):
        largest = struct.unpack("<f", b"\xff\xff\x7f\x7f")[0]
        self.assertEqual(f32(largest * 2), math.inf)
        self.assertEqual(f32(-largest * 2), -math.inf)
        # Under half an ulp past the largest finite single, it rounds
        # down; exactly half is a tie, and the even neighbour is infinity.
        self.assertEqual(f32(largest + 2.0**102), largest)
        self.assertEqual(f32(largest + 2.0**103), math.inf)
        self.assertTrue(math.isnan(f32(math.nan)))

    def test_division_and_sqrt_do_not_raise(self):
        self.assertEqual(fdiv(1.0, 0.0), math.inf)
        self.assertEqual(fdiv(1.0, -0.0), -math.inf)
        self.assertEqual(fdiv(-1.0, 0.0), -math.inf)
        self.assertTrue(math.isnan(fdiv(0.0, 0.0)))
        self.assertTrue(math.isnan(fsqrt(-1.0)))
        self.assertTrue(math.isnan(fsqrt(-math.inf)))
        self.assertEqual(math.copysign(1.0, fsqrt(-0.0)), -1.0)

    def test_nan_comparisons(self):
        """Through the instructions themselves, as a program sees them."""
        program = (
            'import "../../lib/wasm.moss" as w use Wasm;\n'
            'import "../../lib/wasistd.moss" use Branch, i32_bool;\n'
            "assume Std, Wasm, Branch {\n"
            "  fn yes(b: Bool) { if b { putchar(char::y) } else { putchar(char::n) } }\n"
            "  fn main() {\n"
            "    let nan = w::f64_div(w::f64_convert_i32_s(w::i32_zero), w::f64_convert_i32_s(w::i32_zero));\n"
            "    let one = w::f64_convert_i32_s(w::i32_one);\n"
            "    yes(i32_bool(w::f64_eq(nan, nan)));\n"
            "    yes(i32_bool(w::f64_ne(nan, nan)));\n"
            "    yes(i32_bool(w::f64_lt(nan, one)));\n"
            "    yes(i32_bool(w::f64_gt(nan, one)));\n"
            "    yes(i32_bool(w::f64_le(nan, nan)));\n"
            "    yes(i32_bool(w::f64_ge(one, nan)));\n"
            "    let nan32 = w::f32_demote_f64(nan);\n"
            "    yes(i32_bool(w::f32_eq(nan32, nan32)));\n"
            "    yes(i32_bool(w::f32_ne(nan32, nan32)));\n"
            "    yes(i32_bool(w::f64_eq(w::f64_min(nan, one), w::f64_min(nan, one))));\n"
            "  }\n"
            "}\n"
        )
        self.assertEqual(run({"tests/wasi/nan.moss": program}, entry="tests/wasi/nan.moss"),
                         "nynnnnnyn")

    def test_min_max_and_signed_zeros(self):
        self.assertEqual(math.copysign(1.0, fmin(0.0, -0.0)), -1.0)
        self.assertEqual(math.copysign(1.0, fmin(-0.0, 0.0)), -1.0)
        self.assertEqual(math.copysign(1.0, fmax(-0.0, 0.0)), 1.0)
        self.assertTrue(math.isnan(fmin(math.nan, 1.0)))
        self.assertTrue(math.isnan(fmax(1.0, math.nan)))

    def test_rounding_to_integral(self):
        self.assertEqual(fround(2.5, round), 2.0)
        self.assertEqual(fround(3.5, round), 4.0)
        self.assertEqual(fround(-2.5, round), -2.0)
        self.assertEqual(math.copysign(1.0, fround(-0.4, round)), -1.0)
        self.assertEqual(math.copysign(1.0, fround(-0.5, math.ceil)), -1.0)
        self.assertEqual(fround(-2.5, math.floor), -3.0)
        self.assertEqual(fround(math.inf, math.trunc), math.inf)
        self.assertTrue(math.isnan(fround(math.nan, round)))

    def test_truncation_traps_or_saturates(self):
        self.assertEqual(trunc_to_int(-3.9, 32, True, False), -3)
        self.assertEqual(trunc_to_int(3e9, 32, False, False), 3000000000 - 2**32)
        for bad in (math.nan, 2.0**31, -2.0**31 - 1, math.inf):
            with self.assertRaises(MossPanic):
                trunc_to_int(bad, 32, True, False)
        with self.assertRaises(MossPanic):
            trunc_to_int(-1.0, 32, False, False)
        # -0.9 truncates to 0, which is in range even unsigned.
        self.assertEqual(trunc_to_int(-0.9, 32, False, False), 0)
        self.assertEqual(trunc_to_int(math.nan, 32, True, True), 0)
        self.assertEqual(trunc_to_int(1e10, 32, True, True), 2**31 - 1)
        self.assertEqual(trunc_to_int(-1e10, 32, True, True), -(2**31))
        self.assertEqual(trunc_to_int(-1e10, 32, False, True), 0)
        self.assertEqual(trunc_to_int(1e30, 64, False, True), -1)  # all ones
        self.assertEqual(trunc_to_int(-math.inf, 64, True, True), -(2**63))

    def test_integer_conversion_rounds_once(self):
        self.assertEqual(int_to_float(2**53 + 1, 53), 2.0**53)
        self.assertEqual(int_to_float(2**53 + 3, 53), 2.0**53 + 4)
        self.assertEqual(int_to_float(-(2**24) - 1, 24), -(2.0**24))
        # Through a double first, this would tie at single precision and
        # round down to 2^60; rounded once it is above the midpoint.
        n = 2**60 + 2**36 + 1
        self.assertEqual(bits32(f32(float(n))), 0x5D800000)
        self.assertEqual(bits32(int_to_float(n, 24)), 0x5D800001)
        self.assertEqual(int_to_float(0, 24), 0.0)

    def test_reinterpret(self):
        self.assertEqual(reinterpret(1.0, "f", "i"), 0x3F800000)
        self.assertEqual(reinterpret(-1.0, "f", "i"), -0x40800000)
        self.assertEqual(reinterpret(-0x40800000, "i", "f"), -1.0)
        self.assertEqual(reinterpret(2.0, "d", "q"), 0x4000000000000000)
        self.assertEqual(reinterpret(-(2**63), "q", "d"), -0.0)

    def test_the_interpreter_runs_the_float_program(self):
        """tests/wasi/float.moss, which both back ends compile, prints the
        same thing interpreted."""
        self.assertEqual(run({}, entry="tests/wasi/float.moss"), FLOAT_OUT)

    def test_the_interpreter_runs_float_over_wasi(self):
        """`Float` as `WasiStd` and `WasiFloats32` provide it — Moss over
        the emulated instructions, so f32 rounding here is `f32`'s."""
        self.assertEqual(run({}, entry="tests/wasi/newton.moss"), NEWTON_OUT)


if __name__ == "__main__":
    unittest.main()
