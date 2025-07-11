/*
 * QEMU Geforce NV2A pixel shader translation
 *
 * Copyright (c) 2013 espes
 * Copyright (c) 2015 Jannik Vogel
 * Copyright (c) 2020-2024 Matt Borgerson
 *
 * Based on:
 * Cxbx, PixelShader.cpp
 * Copyright (c) 2004 Aaron Robinson <caustik@caustik.com>
 *                    Kingofc <kingofc@freenet.de>
 * Xeon, XBD3DPixelShader.cpp
 * Copyright (c) 2003 _SF_
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 or
 * (at your option) version 3 of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#include "qemu/osdep.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

#include "common.h"
#include "hw/xbox/nv2a/debug.h"
#include "hw/xbox/nv2a/pgraph/psh.h"
#include "ui/xemu-settings.h"
#include "psh.h"

/*
 * This implements translation of register combiners into glsl
 * fragment shaders, but all terminology is in terms of Xbox DirectX
 * pixel shaders, since I wanted to be lazy while referencing existing
 * work / stealing code.
 *
 * For some background, see the OpenGL extension:
 * https://www.opengl.org/registry/specs/NV/register_combiners.txt
 */


enum PS_TEXTUREMODES
{                                 // valid in stage 0 1 2 3
    PS_TEXTUREMODES_NONE=                 0x00L, // * * * *
    PS_TEXTUREMODES_PROJECT2D=            0x01L, // * * * *
    PS_TEXTUREMODES_PROJECT3D=            0x02L, // * * * *
    PS_TEXTUREMODES_CUBEMAP=              0x03L, // * * * *
    PS_TEXTUREMODES_PASSTHRU=             0x04L, // * * * *
    PS_TEXTUREMODES_CLIPPLANE=            0x05L, // * * * *
    PS_TEXTUREMODES_BUMPENVMAP=           0x06L, // - * * *
    PS_TEXTUREMODES_BUMPENVMAP_LUM=       0x07L, // - * * *
    PS_TEXTUREMODES_BRDF=                 0x08L, // - - * *
    PS_TEXTUREMODES_DOT_ST=               0x09L, // - - * *
    PS_TEXTUREMODES_DOT_ZW=               0x0aL, // - - * *
    PS_TEXTUREMODES_DOT_RFLCT_DIFF=       0x0bL, // - - * -
    PS_TEXTUREMODES_DOT_RFLCT_SPEC=       0x0cL, // - - - *
    PS_TEXTUREMODES_DOT_STR_3D=           0x0dL, // - - - *
    PS_TEXTUREMODES_DOT_STR_CUBE=         0x0eL, // - - - *
    PS_TEXTUREMODES_DPNDNT_AR=            0x0fL, // - * * *
    PS_TEXTUREMODES_DPNDNT_GB=            0x10L, // - * * *
    PS_TEXTUREMODES_DOTPRODUCT=           0x11L, // - * * -
    PS_TEXTUREMODES_DOT_RFLCT_SPEC_CONST= 0x12L, // - - - *
    // 0x13-0x1f reserved
};

enum PS_INPUTMAPPING
{
    PS_INPUTMAPPING_UNSIGNED_IDENTITY= 0x00L, // max(0,x)         OK for final combiner
    PS_INPUTMAPPING_UNSIGNED_INVERT=   0x20L, // 1 - max(0,x)     OK for final combiner
    PS_INPUTMAPPING_EXPAND_NORMAL=     0x40L, // 2*max(0,x) - 1   invalid for final combiner
    PS_INPUTMAPPING_EXPAND_NEGATE=     0x60L, // 1 - 2*max(0,x)   invalid for final combiner
    PS_INPUTMAPPING_HALFBIAS_NORMAL=   0x80L, // max(0,x) - 1/2   invalid for final combiner
    PS_INPUTMAPPING_HALFBIAS_NEGATE=   0xa0L, // 1/2 - max(0,x)   invalid for final combiner
    PS_INPUTMAPPING_SIGNED_IDENTITY=   0xc0L, // x                invalid for final combiner
    PS_INPUTMAPPING_SIGNED_NEGATE=     0xe0L, // -x               invalid for final combiner
};

enum PS_REGISTER
{
    PS_REGISTER_ZERO=              0x00L, // r
    PS_REGISTER_DISCARD=           0x00L, // w
    PS_REGISTER_C0=                0x01L, // r
    PS_REGISTER_C1=                0x02L, // r
    PS_REGISTER_FOG=               0x03L, // r
    PS_REGISTER_V0=                0x04L, // r/w
    PS_REGISTER_V1=                0x05L, // r/w
    PS_REGISTER_T0=                0x08L, // r/w
    PS_REGISTER_T1=                0x09L, // r/w
    PS_REGISTER_T2=                0x0aL, // r/w
    PS_REGISTER_T3=                0x0bL, // r/w
    PS_REGISTER_R0=                0x0cL, // r/w
    PS_REGISTER_R1=                0x0dL, // r/w
    PS_REGISTER_V1R0_SUM=          0x0eL, // r
    PS_REGISTER_EF_PROD=           0x0fL, // r

    PS_REGISTER_ONE=               PS_REGISTER_ZERO | PS_INPUTMAPPING_UNSIGNED_INVERT, // OK for final combiner
    PS_REGISTER_NEGATIVE_ONE=      PS_REGISTER_ZERO | PS_INPUTMAPPING_EXPAND_NORMAL,   // invalid for final combiner
    PS_REGISTER_ONE_HALF=          PS_REGISTER_ZERO | PS_INPUTMAPPING_HALFBIAS_NEGATE, // invalid for final combiner
    PS_REGISTER_NEGATIVE_ONE_HALF= PS_REGISTER_ZERO | PS_INPUTMAPPING_HALFBIAS_NORMAL, // invalid for final combiner
};

enum PS_COMBINERCOUNTFLAGS
{
    PS_COMBINERCOUNT_MUX_LSB=     0x0000L, // mux on r0.a lsb
    PS_COMBINERCOUNT_MUX_MSB=     0x0001L, // mux on r0.a msb

    PS_COMBINERCOUNT_SAME_C0=     0x0000L, // c0 same in each stage
    PS_COMBINERCOUNT_UNIQUE_C0=   0x0010L, // c0 unique in each stage

    PS_COMBINERCOUNT_SAME_C1=     0x0000L, // c1 same in each stage
    PS_COMBINERCOUNT_UNIQUE_C1=   0x0100L  // c1 unique in each stage
};

enum PS_COMBINEROUTPUT
{
    PS_COMBINEROUTPUT_IDENTITY=            0x00L, // y = x
    PS_COMBINEROUTPUT_BIAS=                0x08L, // y = x - 0.5
    PS_COMBINEROUTPUT_SHIFTLEFT_1=         0x10L, // y = x*2
    PS_COMBINEROUTPUT_SHIFTLEFT_1_BIAS=    0x18L, // y = (x - 0.5)*2
    PS_COMBINEROUTPUT_SHIFTLEFT_2=         0x20L, // y = x*4
    PS_COMBINEROUTPUT_SHIFTRIGHT_1=        0x30L, // y = x/2

    PS_COMBINEROUTPUT_AB_BLUE_TO_ALPHA=    0x80L, // RGB only

    PS_COMBINEROUTPUT_CD_BLUE_TO_ALPHA=    0x40L, // RGB only

    PS_COMBINEROUTPUT_AB_MULTIPLY=         0x00L,
    PS_COMBINEROUTPUT_AB_DOT_PRODUCT=      0x02L, // RGB only

    PS_COMBINEROUTPUT_CD_MULTIPLY=         0x00L,
    PS_COMBINEROUTPUT_CD_DOT_PRODUCT=      0x01L, // RGB only

    PS_COMBINEROUTPUT_AB_CD_SUM=           0x00L, // 3rd output is AB+CD
    PS_COMBINEROUTPUT_AB_CD_MUX=           0x04L, // 3rd output is MUX(AB,CD) based on R0.a
};

enum PS_CHANNEL
{
    PS_CHANNEL_RGB=   0x00, // used as RGB source
    PS_CHANNEL_BLUE=  0x00, // used as ALPHA source
    PS_CHANNEL_ALPHA= 0x10, // used as RGB or ALPHA source
};


enum PS_FINALCOMBINERSETTING
{
    PS_FINALCOMBINERSETTING_CLAMP_SUM=     0x80, // V1+R0 sum clamped to [0,1]

    PS_FINALCOMBINERSETTING_COMPLEMENT_V1= 0x40, // unsigned invert mapping

    PS_FINALCOMBINERSETTING_COMPLEMENT_R0= 0x20, // unsigned invert mapping
};

enum PS_DOTMAPPING
{                              // valid in stage 0 1 2 3
    PS_DOTMAPPING_ZERO_TO_ONE=         0x00L, // - * * *
    PS_DOTMAPPING_MINUS1_TO_1_D3D=     0x01L, // - * * *
    PS_DOTMAPPING_MINUS1_TO_1_GL=      0x02L, // - * * *
    PS_DOTMAPPING_MINUS1_TO_1=         0x03L, // - * * *
    PS_DOTMAPPING_HILO_1=              0x04L, // - * * *
    PS_DOTMAPPING_HILO_HEMISPHERE_D3D= 0x05L, // - * * *
    PS_DOTMAPPING_HILO_HEMISPHERE_GL=  0x06L, // - * * *
    PS_DOTMAPPING_HILO_HEMISPHERE=     0x07L, // - * * *
};


// Structures to describe the PS definition

struct InputInfo {
    int reg, mod, chan;
};

struct InputVarInfo {
    struct InputInfo a, b, c, d;
};

struct FCInputInfo {
    struct InputInfo a, b, c, d, e, f, g;
    bool v1r0_sum, clamp_sum, inv_v1, inv_r0, enabled;
};

struct OutputInfo {
    int ab, cd, muxsum, flags, ab_op, cd_op, muxsum_op,
        mapping, ab_alphablue, cd_alphablue;
};

struct PSStageInfo {
    struct InputVarInfo rgb_input, alpha_input;
    struct OutputInfo rgb_output, alpha_output;
    int c0, c1;
};

struct PixelShader {
    PshState state;

    int num_stages, flags;
    struct PSStageInfo stage[8];
    struct FCInputInfo final_input;
    int tex_modes[4], input_tex[4], dot_map[4];

    MString *varE, *varF;
    MString *code;
    int cur_stage;

    int num_var_refs;
    char var_refs[32][32];
    int num_const_refs;
    char const_refs[32][32];
};

static void add_var_ref(struct PixelShader *ps, const char *var)
{
    int i;
    for (i=0; i<ps->num_var_refs; i++) {
        if (strcmp((char*)ps->var_refs[i], var) == 0) return;
    }
    strcpy((char*)ps->var_refs[ps->num_var_refs++], var);
}

static void add_const_ref(struct PixelShader *ps, const char *var)
{
    int i;
    for (i=0; i<ps->num_const_refs; i++) {
        if (strcmp((char*)ps->const_refs[i], var) == 0) return;
    }
    strcpy((char*)ps->const_refs[ps->num_const_refs++], var);
}

// Get the code for a variable used in the program
static MString* get_var(struct PixelShader *ps, int reg, bool is_dest)
{
    switch (reg) {
    case PS_REGISTER_DISCARD:
        if (is_dest) {
            return mstring_from_str("");
        } else {
            return mstring_from_str("vec4(0.0)");
        }
        break;
    case PS_REGISTER_C0:
        if (ps->flags & PS_COMBINERCOUNT_UNIQUE_C0 || ps->cur_stage == 8) {
            MString *reg_name = mstring_from_fmt("c0_%d", ps->cur_stage);
            add_const_ref(ps, mstring_get_str(reg_name));
            return reg_name;
        } else {  // Same c0
            add_const_ref(ps, "c0_0");
            return mstring_from_str("c0_0");
        }
        break;
    case PS_REGISTER_C1:
        if (ps->flags & PS_COMBINERCOUNT_UNIQUE_C1 || ps->cur_stage == 8) {
            MString *reg_name = mstring_from_fmt("c1_%d", ps->cur_stage);
            add_const_ref(ps, mstring_get_str(reg_name));
            return reg_name;
        } else {  // Same c1
            add_const_ref(ps, "c1_0");
            return mstring_from_str("c1_0");
        }
        break;
    case PS_REGISTER_FOG:
        return mstring_from_str("pFog");
    case PS_REGISTER_V0:
        return mstring_from_str("v0");
    case PS_REGISTER_V1:
        return mstring_from_str("v1");
    case PS_REGISTER_T0:
        return mstring_from_str("t0");
    case PS_REGISTER_T1:
        return mstring_from_str("t1");
    case PS_REGISTER_T2:
        return mstring_from_str("t2");
    case PS_REGISTER_T3:
        return mstring_from_str("t3");
    case PS_REGISTER_R0:
        add_var_ref(ps, "r0");
        return mstring_from_str("r0");
    case PS_REGISTER_R1:
        add_var_ref(ps, "r1");
        return mstring_from_str("r1");
    case PS_REGISTER_V1R0_SUM:
        add_var_ref(ps, "r0");
        if (ps->final_input.clamp_sum) {
            return mstring_from_fmt(
                    "clamp(vec4(%s.rgb + %s.rgb, 0.0), 0.0, 1.0)",
                    ps->final_input.inv_v1 ? "(1.0 - v1)" : "v1",
                    ps->final_input.inv_r0 ? "(1.0 - r0)" : "r0");
        } else {
            return mstring_from_fmt(
                    "vec4(%s.rgb + %s.rgb, 0.0)",
                    ps->final_input.inv_v1 ? "(1.0 - v1)" : "v1",
                    ps->final_input.inv_r0 ? "(1.0 - r0)" : "r0");
        }
    case PS_REGISTER_EF_PROD:
        return mstring_from_fmt("vec4(%s * %s, 0.0)",
                                mstring_get_str(ps->varE),
                                mstring_get_str(ps->varF));
    default:
        assert(false);
        return NULL;
    }
}

// Get input variable code
static MString* get_input_var(struct PixelShader *ps, struct InputInfo in, bool is_alpha)
{
    MString *reg = get_var(ps, in.reg, false);

    if (!is_alpha) {
        switch (in.chan) {
        case PS_CHANNEL_RGB:
            mstring_append(reg, ".rgb");
            break;
        case PS_CHANNEL_ALPHA:
            mstring_append(reg, ".aaa");
            break;
        default:
            assert(false);
            break;
        }
    } else {
        switch (in.chan) {
        case PS_CHANNEL_BLUE:
            mstring_append(reg, ".b");
            break;
        case PS_CHANNEL_ALPHA:
            mstring_append(reg, ".a");
            break;
        default:
            assert(false);
            break;
        }
    }

    MString *res;
    switch (in.mod) {
    case PS_INPUTMAPPING_UNSIGNED_IDENTITY:
        res = mstring_from_fmt("max(%s, 0.0)", mstring_get_str(reg));
        break;
    case PS_INPUTMAPPING_UNSIGNED_INVERT:
        res = mstring_from_fmt("(1.0 - clamp(%s, 0.0, 1.0))", mstring_get_str(reg));
        break;
    case PS_INPUTMAPPING_EXPAND_NORMAL:
        res = mstring_from_fmt("(2.0 * max(%s, 0.0) - 1.0)", mstring_get_str(reg));
        break;
    case PS_INPUTMAPPING_EXPAND_NEGATE:
        res = mstring_from_fmt("(-2.0 * max(%s, 0.0) + 1.0)", mstring_get_str(reg));
        break;
    case PS_INPUTMAPPING_HALFBIAS_NORMAL:
        res = mstring_from_fmt("(max(%s, 0.0) - 0.5)", mstring_get_str(reg));
        break;
    case PS_INPUTMAPPING_HALFBIAS_NEGATE:
        res = mstring_from_fmt("(-max(%s, 0.0) + 0.5)", mstring_get_str(reg));
        break;
    case PS_INPUTMAPPING_SIGNED_IDENTITY:
        mstring_ref(reg);
        res = reg;
        break;
    case PS_INPUTMAPPING_SIGNED_NEGATE:
        res = mstring_from_fmt("-%s", mstring_get_str(reg));
        break;
    default:
        assert(false);
        break;
    }

    mstring_unref(reg);
    return res;
}

// Get code for the output mapping of a stage
static MString* get_output(MString *reg, int mapping)
{
    MString *res;
    switch (mapping) {
    case PS_COMBINEROUTPUT_IDENTITY:
        mstring_ref(reg);
        res = reg;
        break;
    case PS_COMBINEROUTPUT_BIAS:
        res = mstring_from_fmt("(%s - 0.5)", mstring_get_str(reg));
        break;
    case PS_COMBINEROUTPUT_SHIFTLEFT_1:
        res = mstring_from_fmt("(%s * 2.0)", mstring_get_str(reg));
        break;
    case PS_COMBINEROUTPUT_SHIFTLEFT_1_BIAS:
        res = mstring_from_fmt("((%s - 0.5) * 2.0)", mstring_get_str(reg));
        break;
    case PS_COMBINEROUTPUT_SHIFTLEFT_2:
        res = mstring_from_fmt("(%s * 4.0)", mstring_get_str(reg));
        break;
    case PS_COMBINEROUTPUT_SHIFTRIGHT_1:
        res = mstring_from_fmt("(%s / 2.0)", mstring_get_str(reg));
        break;
    default:
        assert(false);
        break;
    }
    return res;
}

// Add the GLSL code for a stage
static MString* add_stage_code(struct PixelShader *ps,
                               struct InputVarInfo input,
                               struct OutputInfo output,
                               const char *write_mask, bool is_alpha)
{
    MString *ret = mstring_new();
    MString *a = get_input_var(ps, input.a, is_alpha);
    MString *b = get_input_var(ps, input.b, is_alpha);
    MString *c = get_input_var(ps, input.c, is_alpha);
    MString *d = get_input_var(ps, input.d, is_alpha);

    const char *caster = "";
    if (strlen(write_mask) == 3) {
        caster = "vec3";
    }

    MString *ab;
    if (output.ab_op == PS_COMBINEROUTPUT_AB_DOT_PRODUCT) {
        ab = mstring_from_fmt("dot(%s, %s)",
                              mstring_get_str(a), mstring_get_str(b));
    } else {
        ab = mstring_from_fmt("(%s * %s)",
                              mstring_get_str(a), mstring_get_str(b));
    }

    MString *cd;
    if (output.cd_op == PS_COMBINEROUTPUT_CD_DOT_PRODUCT) {
        cd = mstring_from_fmt("dot(%s, %s)",
                              mstring_get_str(c), mstring_get_str(d));
    } else {
        cd = mstring_from_fmt("(%s * %s)",
                              mstring_get_str(c), mstring_get_str(d));
    }

    MString *ab_mapping = get_output(ab, output.mapping);
    MString *cd_mapping = get_output(cd, output.mapping);
    MString *ab_dest = get_var(ps, output.ab, true);
    MString *cd_dest = get_var(ps, output.cd, true);
    MString *muxsum_dest = get_var(ps, output.muxsum, true);

    bool assign_ab = false;
    bool assign_cd = false;
    bool assign_muxsum = false;

    if (mstring_get_length(ab_dest)) {
        mstring_append_fmt(ps->code, "ab.%s = clamp(%s(%s), -1.0, 1.0);\n",
                           write_mask, caster, mstring_get_str(ab_mapping));
        assign_ab = true;
    } else {
        mstring_unref(ab_dest);
        mstring_ref(ab_mapping);
        ab_dest = ab_mapping;
    }

    if (mstring_get_length(cd_dest)) {
        mstring_append_fmt(ps->code, "cd.%s = clamp(%s(%s), -1.0, 1.0);\n",
                           write_mask, caster, mstring_get_str(cd_mapping));
        assign_cd = true;
    } else {
        mstring_unref(cd_dest);
        mstring_ref(cd_mapping);
        cd_dest = cd_mapping;
    }

    MString *muxsum;
    if (output.muxsum_op == PS_COMBINEROUTPUT_AB_CD_SUM) {
        muxsum = mstring_from_fmt("(%s + %s)", mstring_get_str(ab),
                                  mstring_get_str(cd));
    } else {
        muxsum = mstring_from_fmt("((%s) ? %s(%s) : %s(%s))",
                                  (ps->flags & PS_COMBINERCOUNT_MUX_MSB) ?
                                      "r0.a >= 0.5" :
                                      "(uint(r0.a * 255.0) & 1u) == 1u",
                                  caster, mstring_get_str(cd), caster,
                                  mstring_get_str(ab));
    }

    MString *muxsum_mapping = get_output(muxsum, output.mapping);
    if (mstring_get_length(muxsum_dest)) {
        mstring_append_fmt(ps->code, "mux_sum.%s = clamp(%s(%s), -1.0, 1.0);\n",
                           write_mask, caster, mstring_get_str(muxsum_mapping));
        assign_muxsum = true;
    }

    if (assign_ab) {
        mstring_append_fmt(ret, "%s.%s = ab.%s;\n",
                           mstring_get_str(ab_dest), write_mask, write_mask);

        if (!is_alpha && output.flags & PS_COMBINEROUTPUT_AB_BLUE_TO_ALPHA) {
            mstring_append_fmt(ret, "%s.a = ab.b;\n",
                               mstring_get_str(ab_dest));
        }
    }
    if (assign_cd) {
        mstring_append_fmt(ret, "%s.%s = cd.%s;\n",
                           mstring_get_str(cd_dest), write_mask, write_mask);

        if (!is_alpha && output.flags & PS_COMBINEROUTPUT_CD_BLUE_TO_ALPHA) {
            mstring_append_fmt(ret, "%s.a = cd.b;\n",
                               mstring_get_str(cd_dest));
        }
    }
    if (assign_muxsum) {
        mstring_append_fmt(ret, "%s.%s = mux_sum.%s;\n",
                           mstring_get_str(muxsum_dest), write_mask, write_mask);
    }

    mstring_unref(a);
    mstring_unref(b);
    mstring_unref(c);
    mstring_unref(d);
    mstring_unref(ab);
    mstring_unref(cd);
    mstring_unref(ab_mapping);
    mstring_unref(cd_mapping);
    mstring_unref(ab_dest);
    mstring_unref(cd_dest);
    mstring_unref(muxsum_dest);
    mstring_unref(muxsum);
    mstring_unref(muxsum_mapping);

    return ret;
}

// Add code for the final combiner stage
static void add_final_stage_code(struct PixelShader *ps, struct FCInputInfo final)
{
    ps->varE = get_input_var(ps, final.e, false);
    ps->varF = get_input_var(ps, final.f, false);

    MString *a = get_input_var(ps, final.a, false);
    MString *b = get_input_var(ps, final.b, false);
    MString *c = get_input_var(ps, final.c, false);
    MString *d = get_input_var(ps, final.d, false);
    MString *g = get_input_var(ps, final.g, true);

    mstring_append_fmt(ps->code, "fragColor.rgb = %s + mix(vec3(%s), vec3(%s), vec3(%s));\n",
                       mstring_get_str(d), mstring_get_str(c),
                       mstring_get_str(b), mstring_get_str(a));
    mstring_append_fmt(ps->code, "fragColor.a = %s;\n", mstring_get_str(g));

    mstring_unref(a);
    mstring_unref(b);
    mstring_unref(c);
    mstring_unref(d);
    mstring_unref(g);

    mstring_unref(ps->varE);
    mstring_unref(ps->varF);
    ps->varE = ps->varF = NULL;
}

static bool is_x8y24_format(uint8_t color_format)
{
    return color_format == NV097_SET_TEXTURE_FORMAT_COLOR_LU_IMAGE_DEPTH_X8_Y24_FIXED ||
        color_format == NV097_SET_TEXTURE_FORMAT_COLOR_LU_IMAGE_DEPTH_X8_Y24_FLOAT;
}

static const char *get_sampler_type(enum PS_TEXTUREMODES mode, const PshState *state, int i)
{
    const char *sampler2D = "sampler2D";
    const char *sampler3D = "sampler3D";
    const char *samplerCube = "samplerCube";
    int dim = state->dim_tex[i];

    // FIXME: Cleanup
    switch (mode) {
    default:
    case PS_TEXTUREMODES_NONE:
        return NULL;

    case PS_TEXTUREMODES_PROJECT2D:
        if (state->dim_tex[i] == 2) {
            if (state->vulkan && is_x8y24_format(state->tex_color_format[i])) {
                return "usampler2D";
            }
            return sampler2D;
        }
        if (state->dim_tex[i] == 3) return sampler3D;
        assert(!"Unhandled texture dimensions");
        return NULL;

    case PS_TEXTUREMODES_BUMPENVMAP:
    case PS_TEXTUREMODES_BUMPENVMAP_LUM:
    case PS_TEXTUREMODES_DOT_ST:
        if (state->shadow_map[i]) {
            fprintf(stderr, "Shadow map support not implemented for mode %d\n", mode);
            assert(!"Shadow map support not implemented for this mode");
        }
        if (state->dim_tex[i] == 2) return sampler2D;
        if (state->dim_tex[i] == 3 && mode != PS_TEXTUREMODES_DOT_ST) return sampler3D;
        assert(!"Unhandled texture dimensions");
        return NULL;

    case PS_TEXTUREMODES_PROJECT3D:
    case PS_TEXTUREMODES_DOT_STR_3D:
        if (state->vulkan && is_x8y24_format(state->tex_color_format[i])) {
            return "usampler2D";
        }
        if (state->shadow_map[i]) {
            return sampler2D;
        }
        return dim == 2 ? sampler2D : sampler3D;

    case PS_TEXTUREMODES_BRDF:
        assert(dim == 3);
        if (state->shadow_map[i]) {
            fprintf(stderr, "Shadow map support not implemented for mode %d\n", mode);
            assert(!"Shadow map support not implemented for this mode");
        }
        return sampler3D;

    case PS_TEXTUREMODES_CUBEMAP:
    case PS_TEXTUREMODES_DOT_RFLCT_DIFF:
    case PS_TEXTUREMODES_DOT_RFLCT_SPEC:
    case PS_TEXTUREMODES_DOT_RFLCT_SPEC_CONST:
    case PS_TEXTUREMODES_DOT_STR_CUBE:
        if (state->shadow_map[i]) {
            fprintf(stderr, "Shadow map support not implemented for mode %d\n", mode);
            assert(!"Shadow map support not implemented for this mode");
        }
        assert(state->dim_tex[i] == 2);
        return samplerCube;

    case PS_TEXTUREMODES_DPNDNT_AR:
    case PS_TEXTUREMODES_DPNDNT_GB:
        if (state->shadow_map[i]) {
            fprintf(stderr, "Shadow map support not implemented for mode %d\n", mode);
            assert(!"Shadow map support not implemented for this mode");
        }
        assert(state->dim_tex[i] == 2);
        return sampler2D;
    }
}

static const char *shadow_comparison_map[] = {
    [SHADOW_DEPTH_FUNC_LESS] = "<",
    [SHADOW_DEPTH_FUNC_EQUAL] = "==",
    [SHADOW_DEPTH_FUNC_LEQUAL] = "<=",
    [SHADOW_DEPTH_FUNC_GREATER] = ">",
    [SHADOW_DEPTH_FUNC_NOTEQUAL] = "!=",
    [SHADOW_DEPTH_FUNC_GEQUAL] = ">=",
};

static void psh_append_shadowmap(const struct PixelShader *ps, int i, bool compare_z, MString *vars)
{
    if (ps->state.shadow_depth_func == SHADOW_DEPTH_FUNC_NEVER) {
        mstring_append_fmt(vars, "vec4 t%d = vec4(0.0);\n", i);
        return;
    }

    if (ps->state.shadow_depth_func == SHADOW_DEPTH_FUNC_ALWAYS) {
        mstring_append_fmt(vars, "vec4 t%d = vec4(1.0);\n", i);
        return;
    }

    // Depth texture probably should never have signed channels
    assert(!(ps->state.tex_channel_signs[i] & TEX_CHANNEL_SIGNED_MASK));

    g_autofree gchar *normalize_tex_coords = g_strdup_printf("norm%d", i);
    const char *tex_remap = ps->state.rect_tex[i] ? normalize_tex_coords : "";

    const char *comparison = shadow_comparison_map[ps->state.shadow_depth_func];

    bool extract_msb_24b = ps->state.vulkan &&
        is_x8y24_format(ps->state.tex_color_format[i]);

    mstring_append_fmt(
        vars, "%svec4 t%d_depth%s = textureProj(texSamp%d, %s(pT%d.xyw));\n",
        extract_msb_24b ? "u" : "", i, extract_msb_24b ? "_raw" : "", i,
        tex_remap, i);

    if (extract_msb_24b) {
        // TODO: avoid unnecessary divide and multiply below by 0xFFFFFF when
        // compare_z is true.
        mstring_append_fmt(vars,
                           "vec4 t%d_depth = vec4(float(t%d_depth_raw.x >> 8) "
                           "/ 0xFFFFFF, 1.0, 0.0, 0.0);\n",
                           i, i);
    }

    if (compare_z) {
        uint8_t tf = ps->state.tex_color_format[i];

        mstring_append_fmt(
            vars,
            "t%d_depth.x = round(t%d_depth.x * %s);\n"
            "pT%d.z = float(%s(pT%d.z / pT%d.w));\n"
            "vec4 t%d = vec4(t%d_depth.x %s pT%d.z ? 1.0 : 0.0);\n",
            i, i, is_x8y24_format(tf) ? "16777215.0" : "65535.0", i,
            tf == NV097_SET_TEXTURE_FORMAT_COLOR_LU_IMAGE_DEPTH_X8_Y24_FLOAT ? "depthToF24" :
            tf == NV097_SET_TEXTURE_FORMAT_COLOR_LU_IMAGE_DEPTH_X8_Y24_FIXED ? "depthToD24" :
            tf == NV097_SET_TEXTURE_FORMAT_COLOR_LU_IMAGE_DEPTH_Y16_FLOAT ? "depthToF16" : "depthToD16",
            i, i, i, i, comparison, i);
    } else {
        mstring_append_fmt(
            vars,
            "vec4 t%d = vec4(t%d_depth.x %s 0.0 ? 1.0 : 0.0);\n",
            i, i, comparison);
    }
}

// Adjust the s, t coordinates in the given VAR to account for the 4 texel
// border supported by the hardware.
static void apply_border_adjustment(const struct PixelShader *ps, MString *vars, int tex_index, const char *var_template)
{
    int i = tex_index;
    if (ps->state.border_logical_size[i][0] == 0.0f) {
        return;
    }

    char var_name[32] = {0};
    snprintf(var_name, sizeof(var_name), var_template, i);

    mstring_append_fmt(
        vars,
        "vec3 t%dLogicalSize = vec3(%f, %f, %f);\n"
        "%s.xyz = (%s.xyz * t%dLogicalSize + vec3(4, 4, 4)) * vec3(%f, %f, %f);\n",
        i, ps->state.border_logical_size[i][0], ps->state.border_logical_size[i][1], ps->state.border_logical_size[i][2],
        var_name, var_name, i, ps->state.border_inv_real_size[i][0], ps->state.border_inv_real_size[i][1], ps->state.border_inv_real_size[i][2]);
}

static void apply_convolution_filter(const struct PixelShader *ps, MString *vars, int tex)
{
    assert(ps->state.dim_tex[tex] == 2);
    // FIXME: Quincunx

    g_autofree gchar *normalize_tex_coords = g_strdup_printf("norm%d", tex);
    const char *tex_remap = ps->state.rect_tex[tex] ? normalize_tex_coords : "";

    mstring_append_fmt(vars,
        "vec4 t%d = vec4(0.0);\n"
        "for (int i = 0; i < 9; i++) {\n"
        "    vec3 texCoordDelta = vec3(convolution3x3[i], 0);\n"
        "    texCoordDelta.xy /= textureSize(texSamp%d, 0);\n"
        "    t%d += textureProj(texSamp%d, %s(pT%d.xyw) + texCoordDelta) * gaussian3x3[i];\n"
        "}\n", tex, tex, tex, tex, tex_remap, tex);
}

static bool is_yuv_format(uint8_t color_format)
{
    return color_format == NV097_SET_TEXTURE_FORMAT_COLOR_LC_IMAGE_CR8YB8CB8YA8 ||
        color_format == NV097_SET_TEXTURE_FORMAT_COLOR_LC_IMAGE_YB8CR8YA8CB8;
}

static bool is_16bit_format(uint8_t color_format)
{
    return color_format == NV097_SET_TEXTURE_FORMAT_COLOR_LU_IMAGE_Y16 ||
        color_format == NV097_SET_TEXTURE_FORMAT_COLOR_SZ_R16B16 ||
        color_format == NV097_SET_TEXTURE_FORMAT_COLOR_SZ_Y16 ||
        color_format == NV097_SET_TEXTURE_FORMAT_COLOR_LU_IMAGE_R16B16;
}

static void post_process_texture_samples(const struct PixelShader *ps, MString *vars, int tex_index)
{
    uint8_t signs = ps->state.tex_channel_signs[tex_index];
    uint8_t color_format = ps->state.tex_color_format[tex_index];

    mstring_append_fmt(vars, "uvec4 it%d = uvec4(", tex_index);
    if (is_16bit_format(color_format)) {
        mstring_append_fmt(vars, (signs & TEX_CHANNEL_GSIGNED) ?
                           "signed_channel_int_16bit(t%d.g), " : "channel_int_16bit(t%d.b), ", tex_index);
        mstring_append_fmt(vars, (signs & TEX_CHANNEL_ASIGNED) ?
                           "signed_channel_int_16bit(t%d.a)).bgra;\n" : "channel_int_16bit(t%d.r)).bgra;\n", tex_index);
    } else {
        mstring_append_fmt(vars, (signs & TEX_CHANNEL_RSIGNED) ?
                           "signed_channel_int(t%d.r), " : "channel_int(t%d.r), ", tex_index);
        mstring_append_fmt(vars, (signs & TEX_CHANNEL_GSIGNED) ?
                           "signed_channel_int(t%d.g), " : "channel_int(t%d.g), ", tex_index);
        mstring_append_fmt(vars, (signs & TEX_CHANNEL_BSIGNED) ?
                           "signed_channel_int(t%d.b), " : "channel_int(t%d.b), ", tex_index);
        mstring_append_fmt(vars, (signs & TEX_CHANNEL_ASIGNED) ?
                           "signed_channel_int(t%d.a));\n" : "channel_int(t%d.a));\n", tex_index);
    }

    if (signs & TEX_CHANNEL_SIGNED_MASK) {
        mstring_append_fmt(vars, "t%d = vec4(", tex_index);
        if (is_16bit_format(color_format)) {
            mstring_append_fmt(vars, (signs & TEX_CHANNEL_RSIGNED) ? "signed_channel_16bit(t%d.a), " : "t%d.r, ", tex_index);
            mstring_append_fmt(vars, (signs & TEX_CHANNEL_GSIGNED) ? "signed_channel_16bit(t%d.g), " : "t%d.b, ", tex_index);
            mstring_append_fmt(vars, (signs & TEX_CHANNEL_BSIGNED) ? "signed_channel_16bit(t%d.g), " : "t%d.b, ", tex_index);
            mstring_append_fmt(vars, (signs & TEX_CHANNEL_ASIGNED) ? "signed_channel_16bit(t%d.a));\n" : "t%d.r);\n", tex_index);
        } else {
            mstring_append_fmt(vars, (signs & TEX_CHANNEL_RSIGNED) ? "signed_channel(t%d.r), " : "t%d.r, ", tex_index);
            mstring_append_fmt(vars, (signs & TEX_CHANNEL_GSIGNED) ? "signed_channel(t%d.g), " : "t%d.g, ", tex_index);
            mstring_append_fmt(vars, (signs & TEX_CHANNEL_BSIGNED) ? "signed_channel(t%d.b), " : "t%d.b, ", tex_index);
            mstring_append_fmt(vars, (signs & TEX_CHANNEL_ASIGNED) ? "signed_channel(t%d.a));\n" : "t%d.a);\n", tex_index);
        }
    }

    if (is_yuv_format(color_format)) {
        mstring_append_fmt(vars, "t%d = yuv_to_rgb(t%d);\n", tex_index, tex_index);
    }
}

static void set_stage_result_from_rgba(const struct PixelShader *ps, MString *vars, int tex_index)
{
    mstring_append_fmt(vars, "uvec4 it%d = uvec4(channel_int_floor(t%d.r), channel_int_floor(t%d.g), "
                       "channel_int_floor(t%d.b), channel_int_floor(t%d.a));\n",
                       tex_index, tex_index, tex_index, tex_index, tex_index);
}

static MString* psh_convert(struct PixelShader *ps)
{
    bool signed_channel_in_preflight = false;
    bool yuv_to_rgb_in_preflight = false;

    const char *u = ps->state.vulkan ? "" : "uniform "; // FIXME: Remove

    MString *preflight = mstring_new();
    pgraph_get_glsl_vtx_header(preflight, ps->state.vulkan,
                               ps->state.smooth_shading,
                               ps->state.texture_perspective, true, false,
                               false);

    if (ps->state.vulkan) {
        mstring_append_fmt(preflight,
                           "layout(location = 0) out vec4 fragColor;\n"
                           "layout(binding = %d, std140) uniform PshUniforms {\n", PSH_UBO_BINDING);
    } else {
        mstring_append_fmt(preflight,
                           "layout(location = 0) out vec4 fragColor;\n");
    }

    mstring_append_fmt(preflight, "%sint alphaRef;\n"
                                  "%svec4  fogColor;\n"
                                  "%sivec4 clipRegion[8];\n"
                                  "%svec4  clipRange;\n"
                                  "%sfloat depthOffset;\n"
                                  "%sfloat depthFactor;\n"
                                  "%sivec2 surfaceScale;\n"
                                  "%svec3  eyeVec;\n",
                                  u, u, u, u, u, u, u, u);
    if (ps->state.stipple) {
        mstring_append_fmt(preflight, "%suint  stipplePattern[32];\n", u);
    }

    for (int i = 0; i < 4; i++) {
        mstring_append_fmt(preflight, "%smat2  bumpMat%d;\n"
                                      "%sfloat bumpScale%d;\n"
                                      "%sfloat bumpOffset%d;\n"
                                      "%sfloat texScale%d;\n",
                                      u, i, u, i, u, i, u, i);
    }
    for (int i = 0; i < 9; i++) {
        for (int j = 0; j < 2; j++) {
            mstring_append_fmt(preflight, "%svec4 c%d_%d;\n", u, j, i);
        }
    }

    if (ps->state.vulkan) {
        mstring_append(preflight, "};\n");
    }

    const char *dotmap_funcs[] = {
        "dotmap_zero_to_one",
        "dotmap_minus1_to_1_d3d",
        "dotmap_minus1_to_1_gl",
        "dotmap_minus1_to_1",
        "dotmap_hilo_1",
        "dotmap_hilo_hemisphere_d3d",
        "dotmap_hilo_hemisphere_gl",
        "dotmap_hilo_hemisphere",
    };

    mstring_append(preflight,
        "uint channel_int(float uvalue) {\n"
        "    return uint(round(clamp(uvalue, 0.0f, 1.0f)*255.0f));\n"
        "}\n"
        "uint channel_int_floor(float uvalue) {\n"
        "    return uint(clamp(uvalue, 0.0f, 1.0f)*255.0f);\n"
        "}\n"
        "uvec2 channel_int_16bit(float uvalue) {\n"
        "    uint x = uint(round(clamp(uvalue, 0.0f, 1.0f)*65535.0f));\n"
        "    return uvec2(x & 0xFFu, x >> 8);\n"
        "}\n"
        "vec3 dotmap_zero_to_one(uvec4 col) {\n"
        "    return vec3(col.rgb) / 255.0f;\n"
        "}\n"
        "vec3 dotmap_minus1_to_1_d3d(uvec4 col) {\n"
        "    return (vec3(col.rgb) - 128.0f) / 127.0f;\n"
        "}\n"
        "vec3 dotmap_minus1_to_1_gl(uvec4 col) {\n"
        "    vec3 fcol = vec3(col.rgb);\n"
        "    return (fcol + 0.5f - 256.0f*step(127.5f, fcol)) / 127.5f;\n"
        "}\n"
        "vec3 dotmap_minus1_to_1(uvec4 col) {\n"
        "    vec3 fcol = vec3(col.rgb);\n"
        "    return (fcol - 256.0f*step(127.5f, fcol)) / 127.0f;\n"
        "}\n"
        "vec3 dotmap_hilo_1(uvec4 col) {\n"
        "    vec2 hilo = vec2(col.a << 8 | col.r, col.g << 8 | col.b);\n"
        "    return vec3(hilo / 65535.0f, 1.0f);\n"
        "}\n"
        "vec3 dotmap_hilo_hemisphere(uvec4 col) {\n"
        "    vec2 hilo = vec2(col.a << 8 | col.r, col.g << 8 | col.b);\n"
        "    hilo = (hilo - 65536.0f*step(32767.5f, hilo)) / 32767.0f;\n"
        "    return vec3(hilo, sqrt(max(1.0f - dot(hilo, hilo), 0.0f)));\n"
        "}\n"
        "vec3 dotmap_hilo_hemisphere_d3d(uvec4 col) {\n" // Not supported on Xbox
        "    return dotmap_hilo_hemisphere(col);\n"
        "}\n"
        "vec3 dotmap_hilo_hemisphere_gl(uvec4 col) {\n" // Not supported on Xbox
        "    return dotmap_hilo_hemisphere(col);\n"
        "}\n"
        // Kahan's algorithm for computing determinant using FMA for higher
        // precision. See e.g.:
        // Muller et al, "Handbook of Floating-Point Arithmetic", 2nd ed.
        // or
        // Claude-Pierre Jeannerod, Nicolas Louvet, and Jean-Michel Muller,
        // Further analysis of Kahan's algorithm for the accurate
        // computation of 2x2 determinants,
        // Mathematics of Computation 82(284), October 2013.
        "float kahan_det(vec2 a, vec2 b) {\n"
        "  precise float cd = a.y*b.x;\n"
        "  precise float err = fma(-a.y, b.x, cd);\n"
        "  precise float res = fma(a.x, b.y, -cd) + err;\n"
        "  return res;\n"
        "}\n"
        "float area(vec2 a, vec2 b, vec2 c) {\n"
        "  return kahan_det(b - a, c - a);\n"
        "}\n"
        // Convert to floating point, no sign, 4-bit exponent, 12-bit mantissa
        "float depthToF16(float z) {\n"
        "  uint zi = floatBitsToUint(max(z, 0.0)) >> 11;\n"
        "  zi = clamp(zi, 0x78000u, 0x87FFFu) - 0x78000u;\n"
        "  return float(zi < 0x1000u ? 0u : zi);\n" // Flush subnormal numbers to zero
        "}\n"
        // Convert to floating point, no sign, 8-bit exponent, 16-bit mantissa.
        // (Flushing subnormals to zero not needed explicitly since GPU did
        // that to the input 32-bit float already.)
        "float depthToF24(float z) {\n"
        "  return float(floatBitsToUint(max(z, 0.0)) >> 7);\n"
        "}\n"
        // Convert to 16-bit unsigned int. NV2A floors depth value.
        "float depthToD16(float z) {\n"
        "  return clamp(floor(z), 0.0, 65535.0);\n"
        "}\n"
        // Convert to 24-bit unsigned int. NV2A floors depth value.
        "float depthToD24(float z) {\n"
        "  return clamp(floor(z), 0.0, 16777215.0);\n"
        "}\n"
        "const float[9] gaussian3x3 = float[9](\n"
        "    1.0/16.0, 2.0/16.0, 1.0/16.0,\n"
        "    2.0/16.0, 4.0/16.0, 2.0/16.0,\n"
        "    1.0/16.0, 2.0/16.0, 1.0/16.0);\n"
        "const vec2[9] convolution3x3 = vec2[9](\n"
        "    vec2(-1.0,-1.0),vec2(0.0,-1.0),vec2(1.0,-1.0),\n"
        "    vec2(-1.0, 0.0),vec2(0.0, 0.0),vec2(1.0, 0.0),\n"
        "    vec2(-1.0, 1.0),vec2(0.0, 1.0),vec2(1.0, 1.0));\n"
        );

    /* Window Clipping */
    MString *clip = mstring_new();
    mstring_append_fmt(clip, "/*  Window-clip (%slusive) */\n",
                       ps->state.window_clip_exclusive ? "Exc" : "Inc");
    if (!ps->state.window_clip_exclusive) {
        mstring_append(clip, "bool clipContained = false;\n");
    }
    mstring_append(clip, "vec2 coord = gl_FragCoord.xy - 0.5;\n"
                         "for (int i = 0; i < 8; i++) {\n"
                         "  bool outside = any(bvec4(\n"
                         "      lessThan(coord, vec2(clipRegion[i].xy)),\n"
                         "      greaterThanEqual(coord, vec2(clipRegion[i].zw))));\n"
                         "  if (!outside) {\n");
    if (ps->state.window_clip_exclusive) {
        mstring_append(clip, "    discard;\n");
    } else {
        mstring_append(clip, "    clipContained = true;\n"
                             "    break;\n");
    }
    mstring_append(clip, "  }\n"
                         "}\n");
    if (!ps->state.window_clip_exclusive) {
        mstring_append(clip, "if (!clipContained) {\n"
                             "  discard;\n"
                             "}\n");
    }

    if (ps->state.z_perspective) {
        mstring_append(
            clip,
            "vec2 unscaled_xy = gl_FragCoord.xy / surfaceScale;\n"
            "precise float bc0 = area(unscaled_xy, vtxPos1.xy, vtxPos2.xy);\n"
            "precise float bc1 = area(unscaled_xy, vtxPos2.xy, vtxPos0.xy);\n"
            "precise float bc2 = area(unscaled_xy, vtxPos0.xy, vtxPos1.xy);\n"
            "bc0 /= vtxPos0.w;\n"
            "bc1 /= vtxPos1.w;\n"
            "bc2 /= vtxPos2.w;\n"
            "float inv_bcsum = 1.0 / (bc0 + bc1 + bc2);\n"
            // Denominator can be zero in case the rasterized primitive is a
            // point or a degenerate line or triangle.
            "if (isinf(inv_bcsum)) {\n"
            "  inv_bcsum = 0.0;\n"
            "}\n"
            "bc1 *= inv_bcsum;\n"
            "bc2 *= inv_bcsum;\n"
            "precise float zvalue = vtxPos0.w + (bc1*(vtxPos1.w - vtxPos0.w) + bc2*(vtxPos2.w - vtxPos0.w));\n"
            // If GPU clipping is inaccurate, the point gl_FragCoord.xy might
            // be above the horizon of the plane of a rasterized triangle
            // making the interpolated w-coordinate above zero or negative. We
            // should prevent such wrapping through infinity by clamping to
            // infinity.
            "if (zvalue > 0.0) {\n"
            "  float zslopeofs = depthFactor*triMZ*zvalue*zvalue;\n"
            "  zvalue += depthOffset;\n"
            "  zvalue += zslopeofs;\n"
            "} else {\n"
            "  zvalue = uintBitsToFloat(0x7F7FFFFFu);\n"
            "}\n"
            "if (isnan(zvalue)) {\n"
            "  zvalue = uintBitsToFloat(0x7F7FFFFFu);\n"
            "}\n");
    } else {
        mstring_append(
            clip,
            "vec2 unscaled_xy = gl_FragCoord.xy / surfaceScale;\n"
            "precise float bc0 = area(unscaled_xy, vtxPos1.xy, vtxPos2.xy);\n"
            "precise float bc1 = area(unscaled_xy, vtxPos2.xy, vtxPos0.xy);\n"
            "precise float bc2 = area(unscaled_xy, vtxPos0.xy, vtxPos1.xy);\n"
            "float inv_bcsum = 1.0 / (bc0 + bc1 + bc2);\n"
            // Denominator can be zero in case the rasterized primitive is a
            // point or a degenerate line or triangle.
            "if (isinf(inv_bcsum)) {\n"
            "  inv_bcsum = 0.0;\n"
            "}\n"
            "bc1 *= inv_bcsum;\n"
            "bc2 *= inv_bcsum;\n"
            "precise float zvalue = vtxPos0.z + (bc1*(vtxPos1.z - vtxPos0.z) + bc2*(vtxPos2.z - vtxPos0.z));\n"
            "zvalue += depthOffset;\n"
            "zvalue += depthFactor*triMZ;\n");
    }

    /* Depth clipping */
    if (ps->state.depth_clipping) {
        mstring_append(
            clip, "if (zvalue < clipRange.z || clipRange.w < zvalue) {\n"
                  "  discard;\n"
                  "}\n");
    } else {
        mstring_append(
            clip, "zvalue = clamp(zvalue, clipRange.z, clipRange.w);\n");
    }

    if (ps->state.stipple) {
        mstring_append(clip, "if ((stipplePattern[int(gl_FragCoord.y) & 31] & (0x80000000u >> (int(gl_FragCoord.x) & 31))) == 0u) {\n"
                             "  discard;\n"
                             "}\n");
    }

    MString *vars = mstring_new();
    mstring_append(vars, "vec4 pD0 = vtxD0;\n");
    mstring_append(vars, "vec4 pD1 = vtxD1;\n");
    mstring_append(vars, "vec4 pB0 = vtxB0;\n");
    mstring_append(vars, "vec4 pB1 = vtxB1;\n");
    mstring_append(vars, "vec4 pFog = vec4(fogColor.rgb, clamp(vtxFog, 0.0, 1.0));\n");
    mstring_append(vars, "vec4 pT0 = vtxT0;\n");
    mstring_append(vars, "vec4 pT1 = vtxT1;\n");
    mstring_append(vars, "vec4 pT2 = vtxT2;\n");

    if (ps->state.biased_tex[0] || ps->state.biased_tex[1] || ps->state.biased_tex[2] ||
        ps->state.biased_tex[3]) {
        mstring_append_fmt(preflight,
            "vec4 addTextureBias(vec4 v) {\n"
            "  return vec4(v.xy + %d.0/8388608.0*abs(v.xy), v.zw);\n"
            "}\n", g_config.tuning.tex_bias_ulps);
    }

    if (ps->state.biased_tex[0]) {
        mstring_append(vars, "pT0 = addTextureBias(pT0);\n");
    }
    if (ps->state.biased_tex[1]) {
        mstring_append(vars, "pT1 = addTextureBias(pT1);\n");
    }
    if (ps->state.biased_tex[2]) {
        mstring_append(vars, "pT2 = addTextureBias(pT2);\n");
    }

    if (ps->state.point_sprite) {
        assert(!ps->state.rect_tex[3]);
        mstring_append(vars, "vec4 pT3 = vec4(gl_PointCoord, 1.0, 1.0);\n");
    } else {
        mstring_append(vars, "vec4 pT3 = vtxT3;\n");
        if (ps->state.biased_tex[3]) {
            mstring_append(vars, "pT3 = addTextureBias(pT3);\n");
        }
    }
    mstring_append(vars, "\n");
    mstring_append(vars, "vec4 v0 = pD0;\n");
    mstring_append(vars, "vec4 v1 = pD1;\n");
    mstring_append(vars, "vec4 ab;\n");
    mstring_append(vars, "vec4 cd;\n");
    mstring_append(vars, "vec4 mux_sum;\n");

    ps->code = mstring_new();

    for (int i = 0; i < 4; i++) {

        const char *sampler_type = get_sampler_type(ps->tex_modes[i], &ps->state, i);

        g_autofree gchar *normalize_tex_coords = g_strdup_printf("norm%d", i);
        const char *tex_remap = ps->state.rect_tex[i] ? normalize_tex_coords : "";

        assert(ps->dot_map[i] < 8);
        const char *dotmap_func = dotmap_funcs[ps->dot_map[i]];
        if (ps->dot_map[i] > 4) {
            NV2A_UNCONFIRMED("Dot Mapping mode %s", dotmap_func);
        }

        if ((ps->state.tex_channel_signs[i] & TEX_CHANNEL_SIGNED_MASK) && !signed_channel_in_preflight) {
            mstring_append(preflight,
                           "float signed_channel(float uvalue) {\n"
                           "    return clamp((uvalue*255.0f - 128.0f)/127.0f, -1.0f, 1.0f);\n"
                           "}\n"
                           "float signed_channel_16bit(float uvalue) {\n"
                           "    return clamp((uvalue*65535.0f - 32768.0f)/32767.0f, -1.0f, 1.0f);\n"
                           "}\n"
                           "uint signed_channel_int(float uvalue) {\n"
                           "    float x = round(uvalue*255.0f) - 128.0f;\n"
                           "    return uint(x + 256.0f*(1.0f - step(0.0f, x)));\n"
                           "}\n"
                           "uvec2 signed_channel_int_16bit(float uvalue) {\n"
                           "    float x = round(uvalue*65535.0f) - 32768.0f;\n"
                           "    uint xx = uint(x + 65536.0f*(1.0f - step(0.0f, x)));\n"
                           "    return uvec2(xx & 0xFFu, xx >> 8);\n"
                           "}\n"
                );
            signed_channel_in_preflight = true;
        }

        if (is_yuv_format(ps->state.tex_color_format[i]) && !yuv_to_rgb_in_preflight) {
            mstring_append(preflight,
                           "vec4 yuv_to_rgb(vec4 yuv) {\n"
                           "    float c = clamp(yuv.r, 0.0, 1.0) - 16.0/255.0;\n"
                           "    float d = clamp(yuv.g, 0.0, 1.0) - 128.0/255.0;\n"
                           "    float e = clamp(yuv.b, 0.0, 1.0) - 128.0/255.0;\n"
                           "    float r = clamp(1.164 * c + 1.596 * e, 0.0, 1.0);\n"
                           "    float g = clamp(1.164 * c - 0.392 * d - 0.813 * e, 0.0, 1.0);\n"
                           "    float b = clamp(1.164 * c + 2.017 * d, 0.0, 1.0);\n"
                           "    return vec4(r, g, b, yuv.a);\n"
                           "}\n"
                );
            yuv_to_rgb_in_preflight = true;
        }

        switch (ps->tex_modes[i]) {
        case PS_TEXTUREMODES_NONE:
            mstring_append_fmt(vars, "vec4 t%d = vec4(0.0, 0.0, 0.0, 1.0); /* PS_TEXTUREMODES_NONE */\n",
                               i);
            set_stage_result_from_rgba(ps, vars, i);
            break;
        case PS_TEXTUREMODES_PROJECT2D: {
            if (ps->state.shadow_map[i]) {
                psh_append_shadowmap(ps, i, false, vars);
                set_stage_result_from_rgba(ps, vars, i);
            } else {
                apply_border_adjustment(ps, vars, i, "pT%d");
                if (((ps->state.conv_tex[i] == CONVOLUTION_FILTER_GAUSSIAN) ||
                     (ps->state.conv_tex[i] == CONVOLUTION_FILTER_QUINCUNX))) {
                    apply_convolution_filter(ps, vars, i);
                } else {
                    if (ps->state.dim_tex[i] == 2) {
                        mstring_append_fmt(vars, "vec4 t%d = textureProj(texSamp%d, %s(pT%d.xyw));\n",
                                           i, i, tex_remap, i);
                    } else if (ps->state.dim_tex[i] == 3) {
                        mstring_append_fmt(vars, "vec4 t%d = textureProj(texSamp%d, vec4(pT%d.xy, 0.0, pT%d.w));\n",
                                           i, i, i, i);
                    } else {
                        assert(!"Unhandled texture dimensions");
                    }
                }
                post_process_texture_samples(ps, vars, i);
            }
            break;
        }
        case PS_TEXTUREMODES_PROJECT3D:
            if (ps->state.shadow_map[i]) {
                psh_append_shadowmap(ps, i, true, vars);
                set_stage_result_from_rgba(ps, vars, i);
            } else {
                apply_border_adjustment(ps, vars, i, "pT%d");
                mstring_append_fmt(vars, "vec4 t%d = textureProj(texSamp%d, %s(pT%d.xyzw));\n",
                                   i, i, tex_remap, i);
                post_process_texture_samples(ps, vars, i);
            }
            break;
        case PS_TEXTUREMODES_CUBEMAP:
            mstring_append_fmt(vars, "vec4 t%d = texture(texSamp%d, pT%d.xyz);\n",
                               i, i, i);
            post_process_texture_samples(ps, vars, i);
            break;
        case PS_TEXTUREMODES_PASSTHRU:
            assert(ps->state.border_logical_size[i][0] == 0.0f && "Unexpected border texture on passthru");
            mstring_append_fmt(vars, "vec4 t%d = clamp(pT%d, 0.0f, 1.0f);\n", i, i);
            set_stage_result_from_rgba(ps, vars, i);
            break;
        case PS_TEXTUREMODES_CLIPPLANE: {
            int j;
            mstring_append_fmt(vars, "vec4 t%d = vec4(0.0); /* PS_TEXTUREMODES_CLIPPLANE */\n",
                               i);
            for (j = 0; j < 4; j++) {
                mstring_append_fmt(vars, "  if(pT%d.%c %s 0.0) { discard; };\n",
                                   i, "xyzw"[j],
                                   ps->state.compare_mode[i][j] ? ">=" : "<");
            }
            set_stage_result_from_rgba(ps, vars, i);
            break;
        }
        case PS_TEXTUREMODES_BUMPENVMAP:
            assert(i >= 1);
            mstring_append_fmt(vars, "vec2 dsdt%d = bumpMat%d * dotmap_minus1_to_1(it%d).bg;\n",
                i, i, ps->input_tex[i]);
            if (ps->state.dim_tex[i] == 2) {
                mstring_append_fmt(vars, "vec4 t%d = texture(texSamp%d, %s(pT%d.xy + dsdt%d));\n",
                    i, i, tex_remap, i, i);
            } else if (ps->state.dim_tex[i] == 3) {
                // FIXME: Does hardware pass through the r/z coordinate or is it 0?
                mstring_append_fmt(vars, "vec4 t%d = texture(texSamp%d, vec3(pT%d.xy + dsdt%d, pT%d.z));\n",
                    i, i, i, i, i);
            } else {
                assert(!"Unhandled texture dimensions");
            }
            post_process_texture_samples(ps, vars, i);
            break;
        case PS_TEXTUREMODES_BUMPENVMAP_LUM:
            assert(i >= 1);
            mstring_append_fmt(vars, "vec3 dsdtl%d = vec3(dotmap_minus1_to_1(it%d).bg, float(it%d.r)/255.0f);\n",
                i, ps->input_tex[i], ps->input_tex[i]);
            mstring_append_fmt(vars, "dsdtl%d.st = bumpMat%d * dsdtl%d.st;\n", i, i, i);
            if (ps->state.dim_tex[i] == 2) {
                mstring_append_fmt(vars, "vec4 t%d = texture(texSamp%d, %s(pT%d.xy + dsdtl%d.st));\n",
                    i, i, tex_remap, i, i);
            } else if (ps->state.dim_tex[i] == 3) {
                // FIXME: Does hardware pass through the r/z coordinate or is it 0?
                mstring_append_fmt(vars, "vec4 t%d = texture(texSamp%d, vec3(pT%d.xy + dsdtl%d.st, pT%d.z));\n",
                    i, i, i, i, i);
            } else {
                assert(!"Unhandled texture dimensions");
            }
            post_process_texture_samples(ps, vars, i);
            mstring_append_fmt(vars, "t%d = t%d * clamp(bumpScale%d * dsdtl%d.p + bumpOffset%d, 0.0f, 1.0f);\n",
                i, i, i, i, i);
            break;
        case PS_TEXTUREMODES_BRDF:
            assert(i >= 2);
            mstring_append(vars, "/* PS_TEXTUREMODES_BRDF */\n");
            mstring_append_fmt(vars, "vec2 brdf%d_e = dotmap_hilo_1(it%d).xy;\n", i, i - 2);
            mstring_append_fmt(vars, "vec2 brdf%d_l = dotmap_hilo_1(it%d).xy;\n", i, i - 1);
            mstring_append_fmt(vars, "vec3 brdf%d = vec3(brdf%d_e.x, brdf%d_l.x, brdf%d_l.y - brdf%d_e.y);\n",
                i, i, i, i, i);
            mstring_append_fmt(vars, "vec4 t%d = texture(texSamp%d, brdf%d);\n",
                i, i, i);
            post_process_texture_samples(ps, vars, i);
            break;
        case PS_TEXTUREMODES_DOT_ST:
            assert(i >= 2);
            mstring_append_fmt(vars, "/* PS_TEXTUREMODES_DOT_ST */\n");
            mstring_append_fmt(vars,
               "float dot%d = dot(pT%d.xyz, %s(it%d));\n"
               "vec2 dotST%d = vec2(dot%d, dot%d);\n",
                i, i, dotmap_func, ps->input_tex[i], i, i-1, i);

            apply_border_adjustment(ps, vars, i, "dotST%d");
            mstring_append_fmt(vars, "vec4 t%d = texture(texSamp%d, %s(dotST%d));\n",
                i, i, tex_remap, i);
            post_process_texture_samples(ps, vars, i);
            break;
        case PS_TEXTUREMODES_DOT_ZW:
            assert(i >= 2);
            mstring_append_fmt(vars, "/* PS_TEXTUREMODES_DOT_ZW */\n");
            mstring_append_fmt(vars, "float dot%d = dot(pT%d.xyz, %s(it%d));\n",
                i, i, dotmap_func, ps->input_tex[i]);
            mstring_append_fmt(vars, "vec4 t%d = vec4(0.0);\n", i);
            // FIXME: mstring_append_fmt(vars, "gl_FragDepth = t%d.x;\n", i);
            set_stage_result_from_rgba(ps, vars, i);
            NV2A_UNIMPLEMENTED("PS_TEXTUREMODES_DOT_ZW");
            break;
        case PS_TEXTUREMODES_DOT_RFLCT_DIFF:
            assert(i == 2);
            mstring_append_fmt(vars, "/* PS_TEXTUREMODES_DOT_RFLCT_DIFF */\n");
            mstring_append_fmt(vars, "float dot%d = dot(pT%d.xyz, %s(it%d));\n",
                i, i, dotmap_func, ps->input_tex[i]);
            assert(ps->dot_map[i+1] < 8);
            mstring_append_fmt(vars, "float dot%d_n = dot(pT%d.xyz, %s(it%d));\n",
                i, i+1, dotmap_funcs[ps->dot_map[i+1]], ps->input_tex[i+1]);
            mstring_append_fmt(vars, "vec3 n_%d = vec3(dot%d, dot%d, dot%d_n);\n",
                i, i-1, i, i);
            apply_border_adjustment(ps, vars, i, "n_%d");
            mstring_append_fmt(vars, "vec4 t%d = texture(texSamp%d, n_%d);\n",
                i, i, i);
            post_process_texture_samples(ps, vars, i);
            break;
        case PS_TEXTUREMODES_DOT_RFLCT_SPEC:
            assert(i == 3);
            mstring_append_fmt(vars, "/* PS_TEXTUREMODES_DOT_RFLCT_SPEC */\n");
            mstring_append_fmt(vars, "float dot%d = dot(pT%d.xyz, %s(it%d));\n",
                i, i, dotmap_func, ps->input_tex[i]);
            mstring_append_fmt(vars, "vec3 n_%d = vec3(dot%d, dot%d, dot%d);\n",
                i, i-2, i-1, i);
            mstring_append_fmt(vars, "vec3 e_%d = vec3(pT%d.w, pT%d.w, pT%d.w);\n",
                i, i-2, i-1, i);
            mstring_append_fmt(vars, "vec3 rv_%d = 2*n_%d*dot(n_%d,e_%d)/dot(n_%d,n_%d) - e_%d;\n",
                i, i, i, i, i, i, i);
            apply_border_adjustment(ps, vars, i, "rv_%d");
            mstring_append_fmt(vars, "vec4 t%d = texture(texSamp%d, rv_%d);\n",
                i, i, i);
            post_process_texture_samples(ps, vars, i);
            break;
        case PS_TEXTUREMODES_DOT_STR_3D:
            assert(i == 3);
            mstring_append_fmt(vars, "/* PS_TEXTUREMODES_DOT_STR_3D */\n");
            mstring_append_fmt(vars,
               "float dot%d = dot(pT%d.xyz, %s(it%d));\n"
               "vec3 dotSTR%d = vec3(dot%d, dot%d, dot%d);\n",
                i, i, dotmap_func, ps->input_tex[i],
                i, i-2, i-1, i);

            apply_border_adjustment(ps, vars, i, "dotSTR%d");
            mstring_append_fmt(vars, "vec4 t%d = texture(texSamp%d, %s(dotSTR%d%s));\n",
                i, i, tex_remap, i, ps->state.dim_tex[i] == 2 ? ".xy" : "");
            post_process_texture_samples(ps, vars, i);
            break;
        case PS_TEXTUREMODES_DOT_STR_CUBE:
            assert(i == 3);
            mstring_append_fmt(vars, "/* PS_TEXTUREMODES_DOT_STR_CUBE */\n");
            mstring_append_fmt(vars,
               "float dot%d = dot(pT%d.xyz, %s(it%d));\n"
               "vec3 dotSTR%dCube = vec3(dot%d, dot%d, dot%d);\n",
                i, i, dotmap_func, ps->input_tex[i],
                i, i-2, i-1, i);
            apply_border_adjustment(ps, vars, i, "dotSTR%dCube");
            mstring_append_fmt(vars, "vec4 t%d = texture(texSamp%d, dotSTR%dCube);\n",
                i, i, i);
            post_process_texture_samples(ps, vars, i);
            break;
        case PS_TEXTUREMODES_DPNDNT_AR:
            assert(i >= 1);
            assert(!ps->state.rect_tex[i]);
            mstring_append_fmt(vars, "vec2 t%dAR = vec2(it%d.ar)/255.0;\n", i, ps->input_tex[i]);
            apply_border_adjustment(ps, vars, i, "t%dAR");
            mstring_append_fmt(vars, "vec4 t%d = texture(texSamp%d, %s(t%dAR));\n",
                i, i, tex_remap, i);
            post_process_texture_samples(ps, vars, i);
            break;
        case PS_TEXTUREMODES_DPNDNT_GB:
            assert(i >= 1);
            assert(!ps->state.rect_tex[i]);
            mstring_append_fmt(vars, "vec2 t%dGB = vec2(it%d.gb)/255.0;\n", i, ps->input_tex[i]);
            apply_border_adjustment(ps, vars, i, "t%dGB");
            mstring_append_fmt(vars, "vec4 t%d = texture(texSamp%d, %s(t%dGB));\n",
                i, i, tex_remap, i);
            post_process_texture_samples(ps, vars, i);
            break;
        case PS_TEXTUREMODES_DOTPRODUCT:
            assert(i == 1 || i == 2);
            mstring_append_fmt(vars, "/* PS_TEXTUREMODES_DOTPRODUCT */\n");
            mstring_append_fmt(vars, "float dot%d = dot(pT%d.xyz, %s(it%d));\n",
                i, i, dotmap_func, ps->input_tex[i]);
            mstring_append_fmt(vars, "vec4 t%d = vec4(0.0);\n", i);
            break;
        case PS_TEXTUREMODES_DOT_RFLCT_SPEC_CONST:
            assert(i == 3);
            mstring_append_fmt(vars, "/* PS_TEXTUREMODES_DOT_RFLCT_SPEC_CONST */\n");
            mstring_append_fmt(vars, "float dot%d = dot(pT%d.xyz, %s(it%d));\n",
                i, i, dotmap_func, ps->input_tex[i]);
            mstring_append_fmt(vars, "vec3 n_%d = vec3(dot%d, dot%d, dot%d);\n",
                i, i-2, i-1, i);
            mstring_append_fmt(vars, "vec3 rv_%d = 2*n_%d*dot(n_%d,eyeVec)/dot(n_%d,n_%d) - eyeVec;\n",
                i, i, i, i, i);
            apply_border_adjustment(ps, vars, i, "rv_%d");
            mstring_append_fmt(vars, "vec4 t%d = texture(texSamp%d, rv_%d);\n",
                i, i, i);
            post_process_texture_samples(ps, vars, i);
            break;
        default:
            fprintf(stderr, "Unknown ps tex mode: 0x%x\n", ps->tex_modes[i]);
            assert(false);
            break;
        }

        if (sampler_type != NULL) {
            if (ps->state.vulkan) {
                mstring_append_fmt(preflight, "layout(binding = %d) ", PSH_TEX_BINDING + i);
            }
            mstring_append_fmt(preflight, "uniform %s texSamp%d;\n", sampler_type, i);

            /* As this means a texture fetch does happen, do alphakill */
            if (ps->state.alphakill[i]) {
                mstring_append_fmt(vars, "if (t%d.a == 0.0) { discard; };\n",
                                   i);
            }

            if (ps->state.rect_tex[i]) {
                mstring_append_fmt(preflight,
                "vec2 norm%d(vec2 coord) {\n"
                "    return coord / (textureSize(texSamp%d, 0) / texScale%d);\n"
                "}\n",
                i, i, i);
                mstring_append_fmt(preflight,
                "vec3 norm%d(vec3 coord) {\n"
                "    return vec3(norm%d(coord.xy), coord.z);\n"
                "}\n",
                i, i);
                mstring_append_fmt(preflight,
                "vec4 norm%d(vec4 coord) {\n"
                "    return vec4(norm%d(coord.xy), 0, coord.w);\n"
                "}\n",
                i, i);
            }
        }
    }

    for (int i = 0; i < ps->num_stages; i++) {
        ps->cur_stage = i;
        mstring_append_fmt(ps->code, "// Stage %d\n", i);
        MString* color = add_stage_code(ps, ps->stage[i].rgb_input, ps->stage[i].rgb_output, "rgb", false);
        MString* alpha = add_stage_code(ps, ps->stage[i].alpha_input, ps->stage[i].alpha_output, "a", true);

        mstring_append(ps->code, mstring_get_str(color));
        mstring_append(ps->code, mstring_get_str(alpha));
        mstring_unref(color);
        mstring_unref(alpha);
    }

    if (ps->final_input.enabled) {
        ps->cur_stage = 8;
        mstring_append(ps->code, "// Final Combiner\n");
        add_final_stage_code(ps, ps->final_input);
    } else {
        bool r0_used = false;

        for (int i = 0; i < ps->num_var_refs; i++) {
            if (strcmp(ps->var_refs[i], "r0") == 0) {
                r0_used = true;
                break;
            }
        }

        if (r0_used) {
            mstring_append(ps->code, "fragColor = r0;\n");
        } else {
            mstring_append(ps->code, "fragColor = vec4(0.0);\n");
        }
    }

    if (ps->state.alpha_test && ps->state.alpha_func != ALPHA_FUNC_ALWAYS) {
        if (ps->state.alpha_func == ALPHA_FUNC_NEVER) {
            mstring_append(ps->code, "discard;\n");
        } else {
            const char* alpha_op;
            switch (ps->state.alpha_func) {
            case ALPHA_FUNC_LESS: alpha_op = "<"; break;
            case ALPHA_FUNC_EQUAL: alpha_op = "=="; break;
            case ALPHA_FUNC_LEQUAL: alpha_op = "<="; break;
            case ALPHA_FUNC_GREATER: alpha_op = ">"; break;
            case ALPHA_FUNC_NOTEQUAL: alpha_op = "!="; break;
            case ALPHA_FUNC_GEQUAL: alpha_op = ">="; break;
            default:
                assert(false);
                break;
            }
            mstring_append_fmt(ps->code,
                               "int fragAlpha = int(round(fragColor.a * 255.0));\n"
                               "if (!(fragAlpha %s alphaRef)) discard;\n",
                               alpha_op);
        }
    }

    for (int i = 0; i < ps->num_var_refs; i++) {
        mstring_append_fmt(vars, "vec4 %s = vec4(0);\n", ps->var_refs[i]);
        if (strcmp(ps->var_refs[i], "r0") == 0) {
            if (ps->tex_modes[0] != PS_TEXTUREMODES_NONE) {
                mstring_append(vars, "r0.a = t0.a;\n");
            } else {
                mstring_append(vars, "r0.a = 1.0;\n");
            }
        }
    }

    switch (ps->state.depth_format) {
    case DEPTH_FORMAT_D16:
    case DEPTH_FORMAT_F16:
        mstring_append_fmt(
            ps->code,
            "gl_FragDepth = %s(zvalue) / 65535.0;\n",
            ps->state.depth_format == DEPTH_FORMAT_F16 ? "depthToF16" :
            "depthToD16");
        break;
    case DEPTH_FORMAT_D24:
    case DEPTH_FORMAT_F24:
    default:
        /* For 24-bit OpenGL/Vulkan integer depth buffer, we divide the desired
         * depth integer value by 16777216.0, then add 1 in integer bit
         * representation to get the same result as dividing the desired depth
         * integer by 16777215.0 would give. (GPUs can't divide by 16777215.0,
         * only multiply by 1.0/16777215.0 which gives different result due to
         * rounding.)
         */
        mstring_append_fmt(
            ps->code,
            "gl_FragDepth = uintBitsToFloat(floatBitsToUint(%s(zvalue) / 16777216.0) + 1u);\n",
            ps->state.depth_format == DEPTH_FORMAT_F24 ? "depthToF24" :
            "depthToD24");
        break;
    }

    MString *final = mstring_new();
    mstring_append_fmt(final, "#version %d\n\n", ps->state.vulkan ? 450 : 400);
    mstring_append(final, mstring_get_str(preflight));
    mstring_append(final, "void main() {\n");
    mstring_append(final, mstring_get_str(vars));
    mstring_append(final, mstring_get_str(clip));
    mstring_append(final, mstring_get_str(ps->code));
    mstring_append(final, "}\n");

    mstring_unref(preflight);
    mstring_unref(vars);
    mstring_unref(ps->code);

    return final;
}

static void parse_input(struct InputInfo *var, int value)
{
    var->reg = value & 0xF;
    var->chan = value & 0x10;
    var->mod = value & 0xE0;
}

static void parse_combiner_inputs(uint32_t value,
                                struct InputInfo *a, struct InputInfo *b,
                                struct InputInfo *c, struct InputInfo *d)
{
    parse_input(d, value & 0xFF);
    parse_input(c, (value >> 8) & 0xFF);
    parse_input(b, (value >> 16) & 0xFF);
    parse_input(a, (value >> 24) & 0xFF);
}

static void parse_combiner_output(uint32_t value, struct OutputInfo *out)
{
    out->cd = value & 0xF;
    out->ab = (value >> 4) & 0xF;
    out->muxsum = (value >> 8) & 0xF;
    int flags = value >> 12;
    out->flags = flags;
    out->cd_op = flags & 1;
    out->ab_op = flags & 2;
    out->muxsum_op = flags & 4;
    out->mapping = flags & 0x38;
    out->ab_alphablue = flags & 0x80;
    out->cd_alphablue = flags & 0x40;
}

MString *pgraph_gen_psh_glsl(const PshState state)
{
    int i;
    struct PixelShader ps;
    memset(&ps, 0, sizeof(ps));

    ps.state = state;

    ps.num_stages = state.combiner_control & 0xFF;
    ps.flags = state.combiner_control >> 8;
    for (i = 0; i < 4; i++) {
        ps.tex_modes[i] = (state.shader_stage_program >> (i * 5)) & 0x1F;
    }

    ps.dot_map[0] = 0;
    ps.dot_map[1] = (state.other_stage_input >> 0) & 0xf;
    ps.dot_map[2] = (state.other_stage_input >> 4) & 0xf;
    ps.dot_map[3] = (state.other_stage_input >> 8) & 0xf;

    ps.input_tex[0] = -1;
    ps.input_tex[1] = 0;
    ps.input_tex[2] = (state.other_stage_input >> 16) & 0xF;
    ps.input_tex[3] = (state.other_stage_input >> 20) & 0xF;
    for (i = 0; i < ps.num_stages; i++) {
        parse_combiner_inputs(state.rgb_inputs[i],
            &ps.stage[i].rgb_input.a, &ps.stage[i].rgb_input.b,
            &ps.stage[i].rgb_input.c, &ps.stage[i].rgb_input.d);
        parse_combiner_inputs(state.alpha_inputs[i],
            &ps.stage[i].alpha_input.a, &ps.stage[i].alpha_input.b,
            &ps.stage[i].alpha_input.c, &ps.stage[i].alpha_input.d);

        parse_combiner_output(state.rgb_outputs[i], &ps.stage[i].rgb_output);
        parse_combiner_output(state.alpha_outputs[i], &ps.stage[i].alpha_output);
    }

    struct InputInfo blank;
    ps.final_input.enabled = state.final_inputs_0 || state.final_inputs_1;
    if (ps.final_input.enabled) {
        parse_combiner_inputs(state.final_inputs_0,
                              &ps.final_input.a, &ps.final_input.b,
                              &ps.final_input.c, &ps.final_input.d);
        parse_combiner_inputs(state.final_inputs_1,
                              &ps.final_input.e, &ps.final_input.f,
                              &ps.final_input.g, &blank);
        int flags = state.final_inputs_1 & 0xFF;
        ps.final_input.clamp_sum = flags & PS_FINALCOMBINERSETTING_CLAMP_SUM;
        ps.final_input.inv_v1 = flags & PS_FINALCOMBINERSETTING_COMPLEMENT_V1;
        ps.final_input.inv_r0 = flags & PS_FINALCOMBINERSETTING_COMPLEMENT_R0;
    }

    return psh_convert(&ps);
}
