/*
 * QEMU double-word (extended precision) GLSL floating-point arithmetic
 *
 * Intel's integrated GPU (UHD 770) 64-bit float (double-precision) performance
 * is extremely bad -- it probably doesn't have FP64 hardware -- so we make do
 * with 32-bit floats only.
 * TODO: use doubles anyway, ditch this library and don't care about Intel?
 *
 * This library relies on the Fused Multiply-Add (fma) GLSL function and its
 * proper implementation with single rounding. It should be well-supported by
 * GPUs, but at least Mesa OpenGL software renderer (llvmpipe) seems to have a
 * poor implementation of fma with two roundings so a lot of precision is lost
 * with it.
 *
 * Copyright (c) 2025 coldhex
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#include "dwfloat.h"

void dwfloat_append_base(MString *s)
{
    mstring_append(
        s,
        // Prevent compilers from contracting two FMA/FMAC instructions into
        // e.g. a combination of sum and a single FMA. Apparently SPIR-V
        // still doesn't have NoContraction for FMA.
        "float contractionBarrier(float a) {\n"
        "  uint aa = min(floatBitsToUint(a), 0xFFFFFFFEu);\n"
        "  return uintBitsToFloat(aa);\n"
        "}\n"

        // The following double-word floating-point aritmetic
        // operations are from:
        // Muller et al, "Handbook of Floating-Point Arithmetic", 2nd ed.
        // and
        // Mioara Maria Joldes, Jean-Michel Muller, Valentina Popescu.
        // Tight and rigorous error bounds for basic building blocks of double-word arithmetic.
        // ACM Transactions on Mathematical Software, 2017,
        // 44 (2), pp.1 - 27. 10.1145/3121432. hal-01351529v3.

        "vec2 two_sum(float a, float b) {\n"
        "  precise float hi = a + b;\n"
        "  precise float ap = hi - b;\n"
        "  precise float bp = hi - ap;\n"
        "  precise float da = a - ap;\n"
        "  precise float db = b - bp;\n"
        "  precise float lo = da + db;\n"
        "  precise vec2 r = vec2(lo, hi);\n"
        "  return r;\n"
        "}\n"
        "vec2 fast_two_sum(float a, float b) {\n"
        "  precise float hi = a + b;\n"
        "  precise float z = hi - a;\n"
        "  precise float lo = b - z;\n"
        "  precise vec2 r = vec2(lo, hi);\n"
        "  return r;\n"
        "}\n"
        "vec2 dwf_add(vec2 a, vec2 b) {\n"
        "  precise vec2 s = two_sum(a.y, b.y);\n"
        "  precise vec2 t = two_sum(a.x, b.x);\n"
        "  precise float c = s.x + t.y;\n"
        "  precise vec2 v = fast_two_sum(s.y, c);\n"
        "  precise float w = t.x + v.x;\n"
        "  precise vec2 r = fast_two_sum(v.y, w);\n"
        "  return r;\n"
        "}\n"
        "vec2 two_product(float a, float b) {\n"
        "  precise float hi = a*b;\n"
        "  precise float lo = fma(a, b, -hi);\n"
        "  lo = contractionBarrier(lo);\n"
        "  precise vec2 r = vec2(lo, hi);\n"
        "  return r;\n"
        "}\n"
        // "vec2 dwf_mul(vec2 a, vec2 b) {\n"
        // "  precise vec2 c = two_product(a.y, b.y);\n"
        // "  precise float p1 = a.y*b.x;\n"
        // "  precise float p2 = a.x*b.y;\n"
        // "  precise float t = p1 + p2;\n"
        // "  c.x += t;\n"
        // "  return fast_two_sum(c.y, c.x);\n"
        // "}\n"
        "vec2 dwf_mul(vec2 a, float b) {\n"
        "  precise vec2 c = two_product(a.y, b);\n"
        "  precise float t = fma(a.x, b, c.x);\n"
        "  t = contractionBarrier(t);\n"
        "  precise vec2 r = fast_two_sum(c.y, t);\n"
        "  return r;\n"
        "}\n"

        // Karp's Method for High-Precision Division. See:
        // Thall, A. Extended-Precision Floating-Point Numbers for
        // GPU Computation.
        // https://andrewthall.org/papers/df64_qf128.pdf
        "vec2 dwf_div(vec2 b, vec2 a) {\n"
        "  precise float xn = 1.0 / a.y;\n"
        "  precise float yn = b.y*xn;\n"
        "  precise float diff = dwf_add(b, dwf_mul(a, -yn)).y;\n"
        "  precise vec2 prod = two_product(xn, diff);\n"
        "  precise vec2 r = dwf_add(vec2(0.0, yn), prod);\n"
        "  return r;\n"
        "}\n"
        "vec2 dwf_div(float b, float a) {\n"
        "  precise float xn = 1.0 / a;\n"
        "  precise float yn = b*xn;\n"
        "  precise float diff = dwf_add(vec2(0.0, b), two_product(a, -yn)).y;\n"
        "  precise vec2 prod = two_product(xn, diff);\n"
        "  precise vec2 r = dwf_add(vec2(0.0, yn), prod);\n"
        "  return r;\n"
        "}\n"
        );
}

void dwfloat_append_floor(MString *s)
{
    mstring_append(
        s,
        // Double-word flooring method by coldhex
        "float dwf_floor(vec2 a) {\n"
        "  if (isinf(a.y)) {\n"
        "    return a.y;\n"
        "  }\n"
        "  precise float t = floor(a.y);\n"
        "  precise float s = min(a.y - t, uintBitsToFloat(0x3f7fffff));\n"
        "  precise float r = t + floor(s + a.x);\n"
        "  return r;\n"
        "}\n"
        );
}

void dwfloat_append_det(MString *s)
{
    mstring_append(
        s,
        "vec2 dwf_det(float a, float d, float b, float c) {\n"
        "  precise vec2 ad = two_product(a, d);\n"
        "  precise vec2 bc = two_product(-b, c);\n"
        "  precise vec2 r = dwf_add(ad, bc);\n"
        "  return r;\n"
        "}\n"
        "vec2 dwf_det(vec2 a, float d, vec2 b, float c) {\n"
        "  precise vec2 ad = dwf_mul(a, d);\n"
        "  precise vec2 bc = dwf_mul(b, -c);\n"
        "  precise vec2 r = dwf_add(ad, bc);\n"
        "  return r;\n"
        "}\n"
        );
}

void dwfloat_append_dot(MString *s)
{
    mstring_append(
        s,
        "vec2 dwf_dot(vec4 a, vec2 b) {\n"
        "  precise vec2 ab = dwf_mul(a.xy, b.x);\n"
        "  precise vec2 cd = dwf_mul(a.zw, b.y);\n"
        "  precise vec2 r = dwf_add(ab, cd);\n"
        "  return r;\n"
        "}\n"
        );
}
