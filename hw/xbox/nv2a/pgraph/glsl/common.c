/*
 * Geforce NV2A PGRAPH GLSL Shader Generator
 *
 * Copyright (c) 2024-2025 Matt Borgerson
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

#include "common.h"
#include "hw/xbox/nv2a/pgraph/pgraph.h"

#define DECL_UNIFORM_ELEMENT_NAME(type) #type,
const char *uniform_element_type_to_str[] = {
    UNIFORM_ELEMENT_TYPE_X(DECL_UNIFORM_ELEMENT_NAME)
};

MString *pgraph_glsl_get_vtx_header(MString *out, bool location, bool in,
                                    bool prefix, bool array)
{
    const char *in_out_s = in ? "in" : "out";
    const char *float_s = "float";
    const char *vec4_s = "vec4";
    const char *prefix_s = prefix ? "v_" : "";
    const char *suffix_s = array ? "[]" : "";
    const struct {
        const char *type, *name;
    } attr[] = {
        { vec4_s,  "vtxPos0" },
        { vec4_s,  "vtxPos1" },
        { vec4_s,  "vtxPos2" },
        { vec4_s,  "vtxD00" },
        { vec4_s,  "vtxD01" },
        { vec4_s,  "vtxD02" },
        { vec4_s,  "vtxD10" },
        { vec4_s,  "vtxD11" },
        { vec4_s,  "vtxD12" },
        { vec4_s,  "vtxB00" },
        { vec4_s,  "vtxB01" },
        { vec4_s,  "vtxB02" },
        { vec4_s,  "vtxB10" },
        { vec4_s,  "vtxB11" },
        { vec4_s,  "vtxB12" },
        { vec4_s,  "vtxT00" },
        { vec4_s,  "vtxT01" },
        { vec4_s,  "vtxT02" },
        { vec4_s,  "vtxT10" },
        { vec4_s,  "vtxT11" },
        { vec4_s,  "vtxT12" },
        { vec4_s,  "vtxT20" },
        { vec4_s,  "vtxT21" },
        { vec4_s,  "vtxT22" },
        { vec4_s,  "vtxT30" },
        { vec4_s,  "vtxT31" },
        { vec4_s,  "vtxT32" },
        { float_s, "vtxFog0" },
        { float_s, "vtxFog1" },
        { float_s, "vtxFog2" },
        { float_s, "triMZ"  },
    };

    for (int i = 0; i < ARRAY_SIZE(attr); i++) {
        if (location) {
            mstring_append_fmt(out, "layout(location = %d) ", i);
        }
        mstring_append_fmt(out, "flat %s %s %s%s%s;\n", in_out_s, attr[i].type,
                           prefix_s, attr[i].name, suffix_s);
    }

    return out;
}

void pgraph_glsl_set_clip_range_uniform_value(PGRAPHState *pg, float clipRange[4])
{
    float zmax;
    switch (pg->surface_shape.zeta_format) {
    case NV097_SET_SURFACE_FORMAT_ZETA_Z16:
        zmax = pg->surface_shape.z_format ? f16_max : (float)0xFFFF;
        break;
    case NV097_SET_SURFACE_FORMAT_ZETA_Z24S8:
        zmax = pg->surface_shape.z_format ? f24_max : (float)0xFFFFFF;
        break;
    default:
        assert(0);
    }

    uint32_t zclip_min = pgraph_reg_r(pg, NV_PGRAPH_ZCLIPMIN);
    uint32_t zclip_max = pgraph_reg_r(pg, NV_PGRAPH_ZCLIPMAX);

    clipRange[0] = 0;
    clipRange[1] = zmax;
    clipRange[2] = *(float *)&zclip_min;
    clipRange[3] = *(float *)&zclip_max;
}
