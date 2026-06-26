//! Test-only crate that checks `lex.wat` against the real `moss-core` lexer, plus
//! a native Rust port of the same hand-rolled algorithm used for benchmarking.
//!
//! `hand_count` mirrors `lex_selfsource.wat` byte-for-byte in structure (random
//! access over a `&[u8]`, the same if-chain dispatch, the same per-byte
//! char-class predicates, the same i64-packed keyword match). It exists so we can
//! measure "this exact algorithm, compiled natively" and separate the
//! algorithm cost from the wasm-runtime cost.

#[inline(always)]
fn get(src: &[u8], i: usize) -> i32 {
    if i < src.len() {
        src[i] as i32
    } else {
        -1
    }
}

#[inline(always)]
fn is_digit(c: i32) -> bool {
    (c as u32).wrapping_sub(48) < 10
}
#[inline(always)]
fn is_alpha(c: i32) -> bool {
    (c as u32).wrapping_sub(65) < 26 || (c as u32).wrapping_sub(97) < 26 || c == 95
}
#[inline(always)]
fn is_word(c: i32) -> bool {
    is_digit(c) || is_alpha(c)
}
#[inline(always)]
fn is_ws(c: i32) -> bool {
    matches!(c, 32 | 9 | 10 | 13 | 11 | 12)
}

#[inline(always)]
fn kw(acc: u64) -> i32 {
    match acc {
        0x6173 => 30,
        0x617373756d65 => 31,
        0x62696e64 => 32,
        0x636f6e74657874 => 33,
        0x656c7365 => 34,
        0x666e => 35,
        0x6966 => 36,
        0x696d706f7274 => 37,
        0x6c6574 => 38,
        0x74797065 => 39,
        0x757365 => 40,
        0x76616c => 41,
        0x766172 => 42,
        0x7768696c65 => 43,
        _ => 44,
    }
}

#[inline(always)]
fn lex_number(src: &[u8], pos: &mut usize, signed: bool) -> i32 {
    let mut i = *pos + signed as usize;
    while is_digit(get(src, i)) {
        i += 1;
    }
    let mut token = 50; // Int
    let (s0, s1, s2) = (get(src, i), get(src, i + 1), get(src, i + 2));
    if !signed && s0 == 117 {
        // 'u'
        if s1 == 51 && s2 == 50 {
            token = 45;
            i += 3;
        } else if s1 == 54 && s2 == 52 {
            token = 47;
            i += 3;
        } else {
            token = 49;
            i += 1;
        }
    } else if s0 == 105 {
        // 'i'
        if s1 == 51 && s2 == 50 {
            token = 46;
            i += 3;
        } else if s1 == 54 && s2 == 52 {
            token = 48;
            i += 3;
        }
    }
    *pos = i;
    token
}

/// One token id, advancing `pos`. Same protocol as the wasm modules:
/// 0 = Eof, -1 = invalid, otherwise the `Token` discriminant.
fn next_token(src: &[u8], pos: &mut usize) -> i32 {
    // skip trivia
    loop {
        let c = get(src, *pos);
        if is_ws(c) {
            *pos += 1;
            continue;
        }
        if c == 35 {
            *pos += 1;
            loop {
                let d = get(src, *pos);
                if d < 0 || d == 10 {
                    break;
                }
                *pos += 1;
            }
            continue;
        }
        break;
    }

    let c = get(src, *pos);
    if c < 0 {
        return 0;
    }
    let c1 = get(src, *pos + 1);

    if is_alpha(c) {
        let mut i = *pos;
        let mut acc: u64 = 0;
        let mut cnt = 0;
        while is_word(get(src, i)) {
            if cnt < 8 {
                acc = (acc << 8) | get(src, i) as u32 as u64;
            }
            cnt += 1;
            i += 1;
        }
        *pos = i;
        return if cnt > 7 { 44 } else { kw(acc) };
    }
    if is_digit(c) {
        return lex_number(src, pos, false);
    }
    if c == 45 {
        if is_digit(c1) {
            return lex_number(src, pos, true);
        }
        *pos += 1;
        return 9;
    }
    macro_rules! two {
        ($nx:expr, $tok:expr, $base:expr) => {{
            if c1 == $nx {
                *pos += 2;
                return $tok;
            }
            *pos += 1;
            return $base;
        }};
    }
    match c {
        33 => two!(61, 23, 1),
        58 => two!(58, 24, 12),
        61 => two!(61, 27, 15),
        60 => {
            if c1 == 60 {
                *pos += 2;
                return 25;
            }
            if c1 == 61 {
                *pos += 2;
                return 26;
            }
            *pos += 1;
            return 14;
        }
        62 => {
            if c1 == 62 {
                *pos += 2;
                return 29;
            }
            if c1 == 61 {
                *pos += 2;
                return 28;
            }
            *pos += 1;
            return 16;
        }
        37 => {
            *pos += 1;
            return 2;
        }
        38 => {
            *pos += 1;
            return 3;
        }
        40 => {
            *pos += 1;
            return 4;
        }
        41 => {
            *pos += 1;
            return 5;
        }
        42 => {
            *pos += 1;
            return 6;
        }
        43 => {
            *pos += 1;
            return 7;
        }
        44 => {
            *pos += 1;
            return 8;
        }
        46 => {
            *pos += 1;
            return 10;
        }
        47 => {
            *pos += 1;
            return 11;
        }
        59 => {
            *pos += 1;
            return 13;
        }
        91 => {
            *pos += 1;
            return 17;
        }
        93 => {
            *pos += 1;
            return 18;
        }
        94 => {
            *pos += 1;
            return 19;
        }
        123 => {
            *pos += 1;
            return 20;
        }
        124 => {
            *pos += 1;
            return 21;
        }
        125 => {
            *pos += 1;
            return 22;
        }
        39 => {
            // char literal
            if c1 < 0 {
                return -1;
            }
            let n = if (c1 as u32) < 0x80 {
                if c1 == 10 {
                    return -1;
                }
                1
            } else if (c1 as u32) < 0xe0 {
                2
            } else if (c1 as u32) < 0xf0 {
                3
            } else {
                4
            };
            if get(src, *pos + 1 + n) == 39 {
                *pos += 2 + n;
                return 51;
            }
            -1
        }
        34 => {
            // string literal
            let mut i = *pos + 1;
            loop {
                let d = get(src, i);
                if d < 0 {
                    return -1;
                }
                if d == 34 {
                    *pos = i + 1;
                    return 52;
                }
                if d == 92 {
                    let e = get(src, i + 1);
                    if matches!(e, 34 | 92 | 110 | 114 | 116) {
                        i += 2;
                        continue;
                    }
                    return -1;
                }
                if d == 10 {
                    return -1;
                }
                i += 1;
            }
        }
        _ => -1,
    }
}

/// Lex the whole input, returning the token count (Eof/invalid terminator included).
pub fn hand_count(src: &[u8]) -> usize {
    let mut pos = 0;
    let mut count = 0;
    loop {
        let id = next_token(src, &mut pos);
        count += 1;
        if id <= 0 {
            return count;
        }
    }
}

/// The full token-id sequence (for differential testing against logos).
pub fn hand_tokens(src: &[u8]) -> Vec<i64> {
    let mut pos = 0;
    let mut out = Vec::new();
    loop {
        let id = next_token(src, &mut pos) as i64;
        out.push(id);
        if id <= 0 {
            return out;
        }
    }
}
