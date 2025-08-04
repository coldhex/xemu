/*
 * Geforce NV2A PGRAPH GLSL Shader Generator
 *
 * Copyright (c) 2015 espes
 * Copyright (c) 2015 Jannik Vogel
 * Copyright (c) 2020-2025 Matt Borgerson
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

#include "qemu/osdep.h"
#include "hw/xbox/nv2a/pgraph/pgraph.h"
#include "geom.h"

void pgraph_glsl_set_geom_state(PGRAPHState *pg, GeomState *state)
{
    state->primitive_mode = (enum ShaderPrimitiveMode)pg->primitive_mode;

    state->polygon_front_mode = (enum ShaderPolygonMode)GET_MASK(
        pgraph_reg_r(pg, NV_PGRAPH_SETUPRASTER),
        NV_PGRAPH_SETUPRASTER_FRONTFACEMODE);
    state->polygon_back_mode = (enum ShaderPolygonMode)GET_MASK(
        pgraph_reg_r(pg, NV_PGRAPH_SETUPRASTER),
        NV_PGRAPH_SETUPRASTER_BACKFACEMODE);

    if (pgraph_reg_r(pg, NV_PGRAPH_SETUPRASTER)
        & NV_PGRAPH_SETUPRASTER_CULLENABLE) {
        state->cull_face = GET_MASK(pgraph_reg_r(pg, NV_PGRAPH_SETUPRASTER),
                                    NV_PGRAPH_SETUPRASTER_CULLCTRL);
    } else {
        state->cull_face = 0;
    }

    if (pgraph_reg_r(pg, NV_PGRAPH_SETUPRASTER)
        & NV_PGRAPH_SETUPRASTER_FRONTFACE) {
        if (state->cull_face == 1) {
            state->cull_face = 2;
        } else if (state->cull_face == 2) {
            state->cull_face = 1;
        }
    }

    {
        unsigned int aa_width = 1, aa_height = 1;
        pgraph_apply_anti_aliasing_factor(pg, &aa_width, &aa_height);
        state->surface_width = pg->surface_binding_dim.width / aa_width;
        state->surface_height = pg->surface_binding_dim.height / aa_height;
    }

    state->texture_perspective = pgraph_reg_r(pg, NV_PGRAPH_CONTROL_3) &
                                 NV_PGRAPH_CONTROL_3_TEXTURE_PERSPECTIVE_ENABLE;

    state->smooth_shading = GET_MASK(pgraph_reg_r(pg, NV_PGRAPH_CONTROL_3),
                                     NV_PGRAPH_CONTROL_3_SHADEMODE) ==
                            NV_PGRAPH_CONTROL_3_SHADEMODE_SMOOTH;

    state->first_vertex_is_provoking =
        GET_MASK(pgraph_reg_r(pg, NV_PGRAPH_CONTROL_3),
                 NV_PGRAPH_CONTROL_3_PROVOKING_VERTEX) ==
        NV_PGRAPH_CONTROL_3_PROVOKING_VERTEX_FIRST;

    state->z_perspective = pgraph_reg_r(pg, NV_PGRAPH_CONTROL_0) &
                           NV_PGRAPH_CONTROL_0_Z_PERSPECTIVE_ENABLE;

    if (pg->renderer->ops.get_gpu_properties) {
        GPUProperties *gpu_props = pg->renderer->ops.get_gpu_properties();

        switch (state->primitive_mode) {
        case PRIM_TYPE_TRIANGLES:
            state->tri_rot0 = gpu_props->geom_shader_winding.tri;
            state->tri_rot1 = state->tri_rot0;
            break;
        case PRIM_TYPE_TRIANGLE_STRIP:
            state->tri_rot0 = gpu_props->geom_shader_winding.tri_strip0;
            state->tri_rot1 = gpu_props->geom_shader_winding.tri_strip1;
            break;
        case PRIM_TYPE_TRIANGLE_FAN:
        case PRIM_TYPE_POLYGON:
            state->tri_rot0 = gpu_props->geom_shader_winding.tri_fan;
            state->tri_rot1 = state->tri_rot0;
            break;
        default:
            break;
        }
    }
}

static const char *get_vertex_order(int rot)
{
    if (rot == 0) {
        return "ivec3(0, 1, 2)";
    } else if (rot == 1) {
        return "ivec3(2, 0, 1)";
    } else {
        return "ivec3(1, 2, 0)";
    }
}

bool pgraph_glsl_need_geom(const GeomState *state)
{
    /* FIXME: Missing support for 2-sided-poly mode */
    assert(state->polygon_front_mode == state->polygon_back_mode);
    enum ShaderPolygonMode polygon_mode = state->polygon_front_mode;

    switch (state->primitive_mode) {
    case PRIM_TYPE_POINTS:
        return false;
    case PRIM_TYPE_LINES:
    case PRIM_TYPE_LINE_LOOP:
    case PRIM_TYPE_LINE_STRIP:
    case PRIM_TYPE_TRIANGLES:
    case PRIM_TYPE_TRIANGLE_STRIP:
    case PRIM_TYPE_TRIANGLE_FAN:
    case PRIM_TYPE_QUADS:
    case PRIM_TYPE_QUAD_STRIP:
        return true;
    case PRIM_TYPE_POLYGON:
        if (polygon_mode == POLY_MODE_POINT) {
            assert(false);
            return false;
        }
        return true;
    default:
        return false;
    }
}

MString *pgraph_glsl_gen_geom(const GeomState *state, GenGeomGlslOptions opts)
{
    /* FIXME: Missing support for 2-sided-poly mode */
    assert(state->polygon_front_mode == state->polygon_back_mode);
    enum ShaderPolygonMode polygon_mode = state->polygon_front_mode;

    bool need_triz = false;
    bool need_edges = false;
    bool need_reorder = false;
    bool need_line = false;
    const char *layout_in = NULL;
    const char *layout_out = NULL;
    const char *body = NULL;
    const char *provoking_index = "0";

    /* TODO: frontface/backface culling for polygon modes POLY_MODE_LINE and
     * POLY_MODE_POINT.
     */
    switch (state->primitive_mode) {
    case PRIM_TYPE_POINTS: return NULL;
    case PRIM_TYPE_LINES:
    case PRIM_TYPE_LINE_LOOP:
    case PRIM_TYPE_LINE_STRIP:
        provoking_index = state->first_vertex_is_provoking ? "0" : "1";
        need_line = true;
        layout_in = "layout(lines) in;\n";
        layout_out = "layout(line_strip, max_vertices = 2) out;\n";
        body = "  emit_line(0, 1, 0.0);\n";
        break;
    case PRIM_TYPE_TRIANGLES:
    case PRIM_TYPE_TRIANGLE_STRIP:
    case PRIM_TYPE_TRIANGLE_FAN:
        if (state->first_vertex_is_provoking) {
            provoking_index = "v[0]";
        } else if (state->primitive_mode == PRIM_TYPE_TRIANGLE_STRIP) {
            provoking_index = "v[2 - (gl_PrimitiveIDIn & 1)]";
        } else if (state->primitive_mode == PRIM_TYPE_TRIANGLE_FAN) {
            provoking_index = "v[1]";
        } else {
            provoking_index = "v[2]";
        }
        need_triz = true;
        need_reorder = true;
        layout_in = "layout(triangles) in;\n";
        if (polygon_mode == POLY_MODE_FILL) {
            need_edges = true;
            layout_out = "layout(triangle_strip, max_vertices = 6) out;\n";
            body = "  emit_tri(v[0], v[1], v[2]);\n";
        } else if (polygon_mode == POLY_MODE_LINE) {
            need_line = true;
            layout_out = "layout(line_strip, max_vertices = 6) out;\n";
            body = "  float dz = calc_triMZ(v[0], v[1], v[2]);\n"
                   "  emit_line(v[0], v[1], dz);\n"
                   "  emit_line(v[1], v[2], dz);\n"
                   "  emit_line(v[2], v[0], dz);\n";
        } else {
            assert(polygon_mode == POLY_MODE_POINT);
            layout_out = "layout(points, max_vertices = 3) out;\n";
            body = "  float dz = calc_triMZ(v[0], v[1], v[2]);\n"
                   "  emit_vertex(v[0], v[0], v[0], v[0], dz);\n"
                   "  EndPrimitive();\n"
                   "  emit_vertex(v[1], v[1], v[1], v[1], dz);\n"
                   "  EndPrimitive();\n"
                   "  emit_vertex(v[2], v[2], v[2], v[2], dz);\n"
                   "  EndPrimitive();\n";
        }
        break;
    case PRIM_TYPE_QUADS:
        provoking_index = "3";
        need_triz = true;
        layout_in = "layout(lines_adjacency) in;\n";
        if (polygon_mode == POLY_MODE_FILL) {
            need_edges = true;
            layout_out = "layout(triangle_strip, max_vertices = 12) out;\n";
            body = "  emit_tri(0, 1, 2);\n"
                   "  emit_tri(0, 2, 3);\n";
        } else if (polygon_mode == POLY_MODE_LINE) {
            need_line = true;
            layout_out = "layout(line_strip, max_vertices = 8) out;\n";
            body = "  float dz = calc_triMZ(0, 1, 2);\n"
                   "  float dz2 = calc_triMZ(0, 2, 3);\n"
                   "  emit_line(0, 1, dz);\n"
                   "  emit_line(1, 2, dz);\n"
                   "  emit_line(2, 3, dz2);\n"
                   "  emit_line(3, 0, dz2);\n";
        } else {
            assert(polygon_mode == POLY_MODE_POINT);
            layout_out = "layout(points, max_vertices = 4) out;\n";
            body = "  float dz = calc_triMZ(0, 1, 2);\n"
                   "  emit_vertex(0, 0, 0, 0, dz);\n"
                   "  EndPrimitive();\n"
                   "  emit_vertex(1, 1, 1, 1, dz);\n"
                   "  EndPrimitive();\n"
                   "  emit_vertex(2, 2, 2, 2, dz);\n"
                   "  EndPrimitive();\n"
                   "  emit_vertex(3, 3, 3, 3, calc_triMZ(0, 2, 3));\n"
                   "  EndPrimitive();\n";
        }
        break;
    case PRIM_TYPE_QUAD_STRIP:
        provoking_index = "3";
        need_triz = true;
        layout_in = "layout(lines_adjacency) in;\n";
        if (polygon_mode == POLY_MODE_FILL) {
            need_edges = true;
            layout_out = "layout(triangle_strip, max_vertices = 12) out;\n";
            body = "  if ((gl_PrimitiveIDIn & 1) != 0) { return; }\n"
                   "  emit_tri(2, 0, 1);\n"
                   "  emit_tri(2, 1, 3);\n";
        } else if (polygon_mode == POLY_MODE_LINE) {
            need_line = true;
            layout_out = "layout(line_strip, max_vertices = 8) out;\n";
            body = "  if ((gl_PrimitiveIDIn & 1) != 0) { return; }\n"
                   "  float dz = calc_triMZ(2, 0, 1);\n"
                   "  float dz2 = calc_triMZ(2, 1, 3);\n"
                   "  emit_line(0, 1, dz);\n"
                   "  emit_line(1, 3, dz2);\n"
                   "  emit_line(3, 2, dz2);\n"
                   "  emit_line(2, 0, dz);\n";
        } else {
            assert(polygon_mode == POLY_MODE_POINT);
            layout_out = "layout(points, max_vertices = 4) out;\n";
            body = "  if ((gl_PrimitiveIDIn & 1) != 0) { return; }\n"
                   "  float dz = calc_triMZ(2, 0, 1);\n"
                   "  emit_vertex(0, 0, 0, 0, dz);\n"
                   "  EndPrimitive();\n"
                   "  emit_vertex(1, 1, 1, 1, dz);\n"
                   "  EndPrimitive();\n"
                   "  emit_vertex(2, 2, 2, 2, dz);\n"
                   "  EndPrimitive();\n"
                   "  emit_vertex(3, 3, 3, 3, calc_triMZ(2, 1, 3));\n"
                   "  EndPrimitive();\n";
        }
        break;
    case PRIM_TYPE_POLYGON:
        if (polygon_mode == POLY_MODE_FILL) {
            provoking_index = "v[2]";
            need_triz = true;
            need_reorder = true;
            need_edges = true;
            layout_in = "layout(triangles) in;\n";
            layout_out = "layout(triangle_strip, max_vertices = 6) out;\n";
            body = "  emit_tri(v[0], v[1], v[2]);\n";
        } else if (polygon_mode == POLY_MODE_LINE) {
            need_line = true;
            /* FIXME: input here is lines and not triangles so we cannot
             * calculate triangle plane slope. Also, the first vertex of the
             * polygon is unavailable so flat shading provoking vertex is
             * wrong.
             */
            layout_in = "layout(lines) in;\n";
            layout_out = "layout(line_strip, max_vertices = 2) out;\n";
            body = "  emit_line(0, 1, 0.0);\n";
        } else {
            assert(false);
            return NULL;
        }
        break;

    default:
        assert(false);
        return NULL;
    }

    /* generate a geometry shader to support deprecated primitive types */
    assert(layout_in);
    assert(layout_out);
    assert(body);
    MString *output =
        mstring_from_fmt("#version %d\n\n"
                         "%s"
                         "%s"
                         "\n"
                         "#define v_vtxPos v_vtxPos0\n"
                         "#define v_vtxD0 v_vtxD00\n"
                         "#define v_vtxD1 v_vtxD10\n"
                         "#define v_vtxB0 v_vtxB00\n"
                         "#define v_vtxB1 v_vtxB10\n"
                         "#define v_vtxT0 v_vtxT00\n"
                         "#define v_vtxT1 v_vtxT10\n"
                         "#define v_vtxT2 v_vtxT20\n"
                         "#define v_vtxT3 v_vtxT30\n"
                         "#define v_vtxFog v_vtxFog0\n"
                         "\n",
                         opts.vulkan ? 450 : 400, layout_in, layout_out);
    pgraph_glsl_get_vtx_header(output, opts.vulkan, true, true, true);
    pgraph_glsl_get_vtx_header(output, opts.vulkan, false, false, false);

    char vertex_order_buf[80];
    const char *vertex_order_body = "";

    if (need_reorder) {
        /* Input triangle absolute vertex order is not guaranteed by OpenGL
         * or Vulkan, only winding order is. Reorder vertices here to first
         * vertex convention which we assumed above when setting
         * provoking_index. This mostly only matters with flat shading, but
         * we reorder always to get consistent results across GPU vendors
         * regarding floating-point rounding when calculating with vtxPos0/1/2.
         */
        mstring_append(output, "ivec3 v;\n");
        if (state->tri_rot0 == state->tri_rot1) {
            snprintf(vertex_order_buf, sizeof(vertex_order_buf), "  v = %s;\n",
                     get_vertex_order(state->tri_rot0));
        } else {
            snprintf(vertex_order_buf, sizeof(vertex_order_buf),
                     "  v = (gl_PrimitiveIDIn & 1) == 0 ? %s : %s;\n",
                     get_vertex_order(state->tri_rot0),
                     get_vertex_order(state->tri_rot1));
        }
        vertex_order_body = vertex_order_buf;
    }

    if (need_line) {
        mstring_append(
            output,
            "void emit_vertex(int index, int i0, int i1, vec4 pos2, float dz) {\n"
            "  int i2 = i0;\n"
            "  vtxPos2 = pos2;\n");
    } else {
        mstring_append(
            output,
            "void emit_vertex(int index, int i0, int i1, int i2, float dz) {\n"
            "  vtxPos2 = v_vtxPos[i2];\n");
    }

    if (need_edges) {
        mstring_append(
            output,
            "  gl_PointSize = 1.0;\n");
    } else {
        mstring_append(
            output,
            "  gl_Position = gl_in[index].gl_Position;\n"
            "  gl_PointSize = gl_in[index].gl_PointSize;\n"
            "  edge0 = ivec3(0, 0, 1);\n"
            "  edge1 = ivec3(0, 0, 1);\n"
            "  edge2 = ivec3(0, 0, 1);\n");
    }

    mstring_append(
        output,
        "  vtxPos0 = v_vtxPos[i0];\n"
        "  vtxPos1 = v_vtxPos[i1];\n"
        "  vtxT00 = v_vtxT0[i0];\n"
        "  vtxT01 = v_vtxT0[i1];\n"
        "  vtxT02 = v_vtxT0[i2];\n"
        "  vtxT10 = v_vtxT1[i0];\n"
        "  vtxT11 = v_vtxT1[i1];\n"
        "  vtxT12 = v_vtxT1[i2];\n"
        "  vtxT20 = v_vtxT2[i0];\n"
        "  vtxT21 = v_vtxT2[i1];\n"
        "  vtxT22 = v_vtxT2[i2];\n"
        "  vtxT30 = v_vtxT3[i0];\n"
        "  vtxT31 = v_vtxT3[i1];\n"
        "  vtxT32 = v_vtxT3[i2];\n"
        "  vtxFog0 = v_vtxFog[i0];\n"
        "  vtxFog1 = v_vtxFog[i1];\n"
        "  vtxFog2 = v_vtxFog[i2];\n"
        "  triMZ = (isnan(dz) || isinf(dz)) ? 0.0 : dz;\n");

    if (state->smooth_shading) {
        mstring_append(
            output,
            "  vtxD00 = v_vtxD0[i0];\n"
            "  vtxD01 = v_vtxD0[i1];\n"
            "  vtxD02 = v_vtxD0[i2];\n"
            "  vtxD10 = v_vtxD1[i0];\n"
            "  vtxD11 = v_vtxD1[i1];\n"
            "  vtxD12 = v_vtxD1[i2];\n"
            "  vtxB00 = v_vtxB0[i0];\n"
            "  vtxB01 = v_vtxB0[i1];\n"
            "  vtxB02 = v_vtxB0[i2];\n"
            "  vtxB10 = v_vtxB1[i0];\n"
            "  vtxB11 = v_vtxB1[i1];\n"
            "  vtxB12 = v_vtxB1[i2];\n");
    } else {
        mstring_append_fmt(
            output,
            "  vtxD00 = v_vtxD0[%s];\n"
            "  vtxD01 = v_vtxD0[%s];\n"
            "  vtxD02 = v_vtxD0[%s];\n"
            "  vtxD10 = v_vtxD1[%s];\n"
            "  vtxD11 = v_vtxD1[%s];\n"
            "  vtxD12 = v_vtxD1[%s];\n"
            "  vtxB00 = v_vtxB0[%s];\n"
            "  vtxB01 = v_vtxB0[%s];\n"
            "  vtxB02 = v_vtxB0[%s];\n"
            "  vtxB10 = v_vtxB1[%s];\n"
            "  vtxB11 = v_vtxB1[%s];\n"
            "  vtxB12 = v_vtxB1[%s];\n",
            provoking_index, provoking_index, provoking_index,
            provoking_index, provoking_index, provoking_index,
            provoking_index, provoking_index, provoking_index,
            provoking_index, provoking_index, provoking_index);
    }

    mstring_append(
        output,
        "  EmitVertex();\n"
        "}\n");

    if (need_triz) {
        mstring_append(
            output,
            // Kahan's algorithm for computing a*b - c*d using FMA for higher
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
            "}\n");

        if (state->z_perspective) {
            mstring_append(
                output,
                "float calc_triMZ(int i0, int i1, int i2) {\n"
                "  mat2 m = mat2(v_vtxPos[i1].xy - v_vtxPos[i0].xy,\n"
                "                v_vtxPos[i2].xy - v_vtxPos[i0].xy);\n"
                "  precise vec2 b = vec2(v_vtxPos[i0].w - v_vtxPos[i1].w,\n"
                "                        v_vtxPos[i0].w - v_vtxPos[i2].w);\n"
                "  b /= vec2(v_vtxPos[i1].w, v_vtxPos[i2].w) * v_vtxPos[i0].w;\n"
                // The following computes dzx and dzy same as
                // vec2 dz = b * inverse(m);
                "  float det = kahan_det(m[0], m[1]);\n"
                "  float dzx = kahan_det(b, vec2(m[0].y, m[1].y)) / det;\n"
                "  float dzy = kahan_det(vec2(m[0].x, m[1].x), b) / det;\n"
                "  return max(abs(dzx), abs(dzy));\n"
                "}\n");
        } else {
            mstring_append(
                output,
                "float calc_triMZ(int i0, int i1, int i2) {\n"
                "  mat2 m = mat2(v_vtxPos[i1].xy - v_vtxPos[i0].xy,\n"
                "                v_vtxPos[i2].xy - v_vtxPos[i0].xy);\n"
                "  precise vec2 b = vec2(v_vtxPos[i1].z - v_vtxPos[i0].z,\n"
                "                        v_vtxPos[i2].z - v_vtxPos[i0].z);\n"
                // The following computes dzx and dzy same as
                // vec2 dz = b * inverse(m);
                "  float det = kahan_det(m[0], m[1]);\n"
                "  float dzx = kahan_det(b, vec2(m[0].y, m[1].y)) / det;\n"
                "  float dzy = kahan_det(vec2(m[0].x, m[1].x), b) / det;\n"
                "  return max(abs(dzx), abs(dzy));\n"
                "}\n");
        }
    }

    if (need_line) {
        mstring_append(
            output,
            // Calculate a third vertex by rotating 90 degrees so that triangle
            // interpolation in fragment shader can be used as is for lines.
            "void emit_line(int i0, int i1, float dz) {\n"
            "  vec2 delta = v_vtxPos[i1].xy - v_vtxPos[i0].xy;\n"
            "  vec2 v2 = vec2(-delta.y, delta.x) + v_vtxPos[i0].xy;\n"
            "  vec4 pos = vec4(v2, v_vtxPos[i0].zw);\n"
            "  emit_vertex(i0, i0, i1, pos, dz);\n"
            "  emit_vertex(i1, i0, i1, pos, dz);\n"
            "  EndPrimitive();\n"
            "}\n");
    } else if (need_edges) {
        mstring_append(
            output,
            "ivec3 calc_edge_coeffs(int i0, int i1) {\n"
            "  vec2 v1 = v_vtxPos[i0].xy * 16.0;\n"
            "  vec2 v2 = v_vtxPos[i1].xy * 16.0;\n"
            "  float M = max(abs(v1.y - v2.y), abs(v2.x - v1.x));\n"
            "  const float clip_at = 2.0*16.0*1000.0;\n"
            "  M = min(clip_at / M, 1.0);\n"
            "  float A = clamp((v1.y - v2.y) * M, -clip_at, clip_at);\n"
            "  float B = clamp((v2.x - v1.x) * M, -clip_at, clip_at);\n"
            "  float C = clamp(kahan_det(v1, v2) * M, -clip_at*clip_at, clip_at*clip_at);\n"
            "  return ivec3(int(round(A)), int(round(B)), int(round(C)));\n"
            "}\n"
            "int calc_winding(ivec2 n0, ivec2 n1) {\n"
            "  return sign(n0.x*n1.y - n0.y*n1.x);\n"
            "}\n"
            "void emit_tri(int i0, int i1, int i2) {\n");
        if (state->cull_face != 3) {
            mstring_append_fmt(
                output,
                "  if (!(v_vtxPos[i0].w > 0.0 || v_vtxPos[i1].w > 0.0 || v_vtxPos[i2].w > 0.0)) {\n"
                "    return;\n"
                "  }\n"
                "  ivec3 e0 = calc_edge_coeffs(i0, i1);\n"
                "  ivec3 e1 = calc_edge_coeffs(i1, i2);\n"
                "  ivec3 e2 = calc_edge_coeffs(i2, i0);\n"
                "  int s0 = calc_winding(e0.xy, e1.xy);\n"
                "  int s1 = calc_winding(e1.xy, e2.xy);\n"
                "  int s2 = calc_winding(e2.xy, e0.xy);\n"
                "  s0 = sign(s0 + s1 + s2);\n"
                "  if (s0 == 0) {\n"
                "    return;\n"
                "  }\n");
        }

        if (state->cull_face == 1 || state->cull_face == 2) {
            mstring_append_fmt(
                output,
                "  int wsign = int(sign(v_vtxPos[i0].w)*sign(v_vtxPos[i1].w)*sign(v_vtxPos[i2].w));\n"
                "  if (!(s0*wsign %c 0)) {\n"
                "    return;\n"
                "  }\n",
                state->cull_face == 1 ? '<' : '>');
        }

        if (state->cull_face != 3) {
            mstring_append_fmt(
                output,
                "  vec2 r0 = vec2(-1.0);\n"
                "  vec2 r1 = vec2(1.0);\n"
                "  if (v_vtxPos[i0].w > 0.0 && v_vtxPos[i1].w > 0.0 && v_vtxPos[i2].w > 0.0) {\n"
                "    r0 = (min(min(floor(v_vtxPos[i0].xy), floor(v_vtxPos[i1].xy)), floor(v_vtxPos[i2].xy)) - vec2(%d.0, %d.0)) / vec2(%d.0, %d.0);\n"
                "    r1 = (max(max(ceil(v_vtxPos[i0].xy), ceil(v_vtxPos[i1].xy)), ceil(v_vtxPos[i2].xy)) - vec2(%d.0, %d.0)) / vec2(%d.0, %d.0);\n"
                "    r0 = max(r0, -1.0);\n"
                "    r1 = min(r1, 1.0);\n"
                "  }\n"
                "  if (r1.x <= -1.0 || r1.y <= -1.0 || r0.x >= 1.0 || r0.y >= 1.0) {\n"
                "    return;\n"
                "  }\n"
                "  e0 *= s0*int(sign(v_vtxPos[i2].w));\n"
                "  e1 *= s0*int(sign(v_vtxPos[i0].w));\n"
                "  e2 *= s0*int(sign(v_vtxPos[i1].w));\n"
                // Top-left rasterization rule: add 1 if top edge
                "  if (e0.x == 0 && e0.y > 0) {\n"
                "    e0.z += 1;\n"
                "  }\n"
                "  if (e1.x == 0 && e1.y > 0) {\n"
                "    e1.z += 1;\n"
                "  }\n"
                "  if (e2.x == 0 && e2.y > 0) {\n"
                "    e2.z += 1;\n"
                "  }\n"
                // Top-left rasterization rule: add 1 if left edge
                "  if (e0.x > 0) {\n"
                "    e0.z += 1;\n"
                "  }\n"
                "  if (e1.x > 0) {\n"
                "    e1.z += 1;\n"
                "  }\n"
                "  if (e2.x > 0) {\n"
                "    e2.z += 1;\n"
                "  }\n"
                "  float dz = calc_triMZ(i0, i1, i2);\n"
                "  gl_Position = vec4(r0.x, r0.y, 0.0, 1.0);\n"
                "  edge0 = e0;\n"
                "  edge1 = e1;\n"
                "  edge2 = e2;\n"
                "  emit_vertex(0, i0, i1, i2, dz);\n"
                "  gl_Position = vec4(r0.x, r1.y, 0.0, 1.0);\n"
                "  edge0 = e0;\n"
                "  edge1 = e1;\n"
                "  edge2 = e2;\n"
                "  emit_vertex(0, i0, i1, i2, dz);\n"
                "  gl_Position = vec4(r1.x, r1.y, 0.0, 1.0);\n"
                "  edge0 = e0;\n"
                "  edge1 = e1;\n"
                "  edge2 = e2;\n"
                "  emit_vertex(0, i0, i1, i2, dz);\n"
                "  EndPrimitive();\n"
                "  gl_Position = vec4(r0.x, r0.y, 0.0, 1.0);\n"
                "  edge0 = e0;\n"
                "  edge1 = e1;\n"
                "  edge2 = e2;\n"
                "  emit_vertex(0, i0, i1, i2, dz);\n"
                "  gl_Position = vec4(r1.x, r1.y, 0.0, 1.0);\n"
                "  edge0 = e0;\n"
                "  edge1 = e1;\n"
                "  edge2 = e2;\n"
                "  emit_vertex(0, i0, i1, i2, dz);\n"
                "  gl_Position = vec4(r1.x, r0.y, 0.0, 1.0);\n"
                "  edge0 = e0;\n"
                "  edge1 = e1;\n"
                "  edge2 = e2;\n"
                "  emit_vertex(0, i0, i1, i2, dz);\n"
                "  EndPrimitive();\n",
                state->surface_width / 2, state->surface_height / 2,
                state->surface_width / 2, state->surface_height / 2,
                state->surface_width / 2, state->surface_height / 2,
                state->surface_width / 2, state->surface_height / 2);
        }
        mstring_append(output, "}\n");
    }

    mstring_append_fmt(output,
                       "\n"
                       "void main() {\n"
                       "%s"
                       "%s"
                       "}\n",
                       vertex_order_body, body);

    return output;
}
