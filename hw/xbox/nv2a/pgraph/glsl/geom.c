/*
 * Geforce NV2A PGRAPH GLSL Shader Generator
 *
 * Copyright (c) 2015 espes
 * Copyright (c) 2015 Jannik Vogel
 * Copyright (c) 2020-2024 Matt Borgerson
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

#include "hw/xbox/nv2a/pgraph/shaders.h"
#include "common.h"
#include "dwfloat.h"
#include "geom.h"

MString *pgraph_gen_geom_glsl(const ShaderState *state)
{
    /* FIXME: Missing support for 2-sided-poly mode */
    assert(state->polygon_front_mode == state->polygon_back_mode);
    enum ShaderPolygonMode polygon_mode = state->polygon_front_mode;

    bool need_triz = false;
    bool need_quadz = false;
    bool need_linez = false;
    const char *layout_in = NULL;
    const char *layout_out = NULL;
    const char *body = NULL;

    switch (state->primitive_mode) {
    case PRIM_TYPE_POINTS: return NULL;
    case PRIM_TYPE_LINES:
    case PRIM_TYPE_LINE_LOOP:
    case PRIM_TYPE_LINE_STRIP:
        need_linez = true;
        layout_in = "layout(lines) in;\n";
        layout_out = "layout(line_strip, max_vertices = 2) out;\n";
        body = "  mat3x4 pz = calc_linez(0, 1);\n"
               "  emit_vertex(0, 0, pz);\n"
               "  emit_vertex(1, 1, pz);\n"
               "  EndPrimitive();\n";
        break;
    case PRIM_TYPE_TRIANGLES:
        need_triz = true;
        layout_in = "layout(triangles) in;\n";
        if (polygon_mode == POLY_MODE_FILL) {
            layout_out = "layout(triangle_strip, max_vertices = 3) out;\n";
            body = "  mat3x4 pz = calc_triz(0, 1, 2);\n"
                   "  emit_vertex(0, 0, pz);\n"
                   "  emit_vertex(1, 1, pz);\n"
                   "  emit_vertex(2, 2, pz);\n"
                   "  EndPrimitive();\n";
        } else if (polygon_mode == POLY_MODE_LINE) {
            layout_out = "layout(line_strip, max_vertices = 4) out;\n";
            body = "  mat3x4 pz = calc_triz(0, 1, 2);\n"
                   "  emit_vertex(0, 0, pz);\n"
                   "  emit_vertex(1, 0, pz);\n"
                   "  emit_vertex(2, 0, pz);\n"
                   "  emit_vertex(0, 0, pz);\n"
                   "  EndPrimitive();\n";
        } else {
            assert(polygon_mode == POLY_MODE_POINT);
            layout_out = "layout(points, max_vertices = 3) out;\n";
            body = "  mat3x4 pz = calc_triz(0, 1, 2);\n"
                   "  emit_vertex(0, 0, pz);\n"
                   "  EndPrimitive();\n"
                   "  emit_vertex(1, 0, pz);\n"
                   "  EndPrimitive();\n"
                   "  emit_vertex(2, 0, pz);\n"
                   "  EndPrimitive();\n";
        }
        break;
    case PRIM_TYPE_TRIANGLE_STRIP:
        need_triz = true;
        layout_in = "layout(triangles) in;\n";
        if (polygon_mode == POLY_MODE_FILL) {
            layout_out = "layout(triangle_strip, max_vertices = 3) out;\n";
            body = "  mat3x4 pz = calc_triz(0, 1, 2);\n"
                   "  emit_vertex(0, 0, pz);\n"
                   "  emit_vertex(1, 1, pz);\n"
                   "  emit_vertex(2, 2, pz);\n"
                   "  EndPrimitive();\n";
        } else if (polygon_mode == POLY_MODE_LINE) {
            layout_out = "layout(line_strip, max_vertices = 4) out;\n";
            body = "  mat3x4 pz = calc_triz(0, 1, 2);\n"
                   "  if (gl_PrimitiveIDIn == 0) {\n"
                   "    emit_vertex(0, 0, pz);\n"
                   "  }\n"
                   "  emit_vertex(1, 0, pz);\n"
                   "  emit_vertex(2, 0, pz);\n"
                   "  emit_vertex(0, 0, pz);\n"
                   "  EndPrimitive();\n";
        } else {
            assert(polygon_mode == POLY_MODE_POINT);
            layout_out = "layout(points, max_vertices = 3) out;\n";
            body = "  mat3x4 pz = calc_triz(0, 1, 2);\n"
                   "  if (gl_PrimitiveIDIn == 0) {\n"
                   "    emit_vertex(0, 0, pz);\n"
                   "    EndPrimitive();\n"
                   "    emit_vertex(1, 0, pz);\n"
                   "    EndPrimitive();\n"
                   "  }\n"
                   "  emit_vertex(2, 0, pz);\n"
                   "  EndPrimitive();\n";
        }
        break;
    case PRIM_TYPE_TRIANGLE_FAN:
        need_triz = true;
        layout_in = "layout(triangles) in;\n";
        if (polygon_mode == POLY_MODE_FILL) {
            layout_out = "layout(triangle_strip, max_vertices = 3) out;\n";
            body = "  mat3x4 pz = calc_triz(0, 1, 2);\n"
                   "  emit_vertex(0, 0, pz);\n"
                   "  emit_vertex(1, 1, pz);\n"
                   "  emit_vertex(2, 2, pz);\n"
                   "  EndPrimitive();\n";
        } else if (polygon_mode == POLY_MODE_LINE) {
            layout_out = "layout(line_strip, max_vertices = 4) out;\n";
            body = "  mat3x4 pz = calc_triz(0, 1, 2);\n"
                   "  if (gl_PrimitiveIDIn == 0) {\n"
                   "    emit_vertex(0, 0, pz);\n"
                   "  }\n"
                   "  emit_vertex(1, 0, pz);\n"
                   "  emit_vertex(2, 0, pz);\n"
                   "  emit_vertex(0, 0, pz);\n"
                   "  EndPrimitive();\n";
        } else {
            assert(polygon_mode == POLY_MODE_POINT);
            layout_out = "layout(points, max_vertices = 3) out;\n";
            body = "  mat3x4 pz = calc_triz(0, 1, 2);\n"
                   "  if (gl_PrimitiveIDIn == 0) {\n"
                   "    emit_vertex(0, 0, pz);\n"
                   "    EndPrimitive();\n"
                   "    emit_vertex(1, 0, pz);\n"
                   "    EndPrimitive();\n"
                   "  }\n"
                   "  emit_vertex(2, 0, pz);\n"
                   "  EndPrimitive();\n";
        }
        break;
    case PRIM_TYPE_QUADS:
        need_quadz = true;
        layout_in = "layout(lines_adjacency) in;\n";
        if (polygon_mode == POLY_MODE_LINE) {
            layout_out = "layout(line_strip, max_vertices = 5) out;\n";
            body = "  mat3x4 pz, pz2;\n"
                   "  calc_quadz(0, 1, 2, 3, pz, pz2);\n"
                   "  emit_vertex(0, 3, pz);\n"
                   "  emit_vertex(1, 3, pz);\n"
                   "  emit_vertex(2, 3, pz);\n"
                   "  emit_vertex(3, 3, pz2);\n"
                   "  emit_vertex(0, 3, pz2);\n"
                   "  EndPrimitive();\n";
        } else if (polygon_mode == POLY_MODE_FILL) {
            layout_out = "layout(triangle_strip, max_vertices = 4) out;\n";
            body = "  mat3x4 pz, pz2;\n"
                   "  calc_quadz(0, 1, 2, 3, pz, pz2);\n"
                   "  emit_vertex(1, 3, pz);\n"
                   "  emit_vertex(2, 3, pz2);\n"
                   "  emit_vertex(0, 3, pz);\n"
                   "  emit_vertex(3, 3, pz2);\n"
                   "  EndPrimitive();\n";
        } else {
            assert(polygon_mode == POLY_MODE_POINT);
            layout_out = "layout(points, max_vertices = 4) out;\n";
            body = "  mat3x4 pz, pz2;\n"
                   "  calc_quadz(0, 1, 2, 3, pz, pz2);\n"
                   "  emit_vertex(0, 3, pz);\n"
                   "  EndPrimitive();\n"
                   "  emit_vertex(1, 3, pz);\n"
                   "  EndPrimitive();\n"
                   "  emit_vertex(2, 3, pz);\n"
                   "  EndPrimitive();\n"
                   "  emit_vertex(3, 3, pz2);\n"
                   "  EndPrimitive();\n";
        }
        break;
    case PRIM_TYPE_QUAD_STRIP:
        need_quadz = true;
        layout_in = "layout(lines_adjacency) in;\n";
        if (polygon_mode == POLY_MODE_LINE) {
            layout_out = "layout(line_strip, max_vertices = 5) out;\n";
            body = "  if ((gl_PrimitiveIDIn & 1) != 0) { return; }\n"
                   "  mat3x4 pz, pz2;\n"
                   "  calc_quadz(2, 0, 1, 3, pz, pz2);\n"
                   "  if (gl_PrimitiveIDIn == 0) {\n"
                   "    emit_vertex(0, 3, pz);\n"
                   "  }\n"
                   "  emit_vertex(1, 3, pz);\n"
                   "  emit_vertex(3, 3, pz2);\n"
                   "  emit_vertex(2, 3, pz2);\n"
                   "  emit_vertex(0, 3, pz);\n"
                   "  EndPrimitive();\n";
        } else if (polygon_mode == POLY_MODE_FILL) {
            layout_out = "layout(triangle_strip, max_vertices = 4) out;\n";
            body = "  if ((gl_PrimitiveIDIn & 1) != 0) { return; }\n"
                   "  mat3x4 pz, pz2;\n"
                   "  calc_quadz(2, 0, 1, 3, pz, pz2);\n"
                   "  emit_vertex(0, 3, pz);\n"
                   "  emit_vertex(1, 3, pz2);\n"
                   "  emit_vertex(2, 3, pz);\n"
                   "  emit_vertex(3, 3, pz2);\n"
                   "  EndPrimitive();\n";
        } else {
            assert(polygon_mode == POLY_MODE_POINT);
            layout_out = "layout(points, max_vertices = 4) out;\n";
            body = "  if ((gl_PrimitiveIDIn & 1) != 0) { return; }\n"
                   "  mat3x4 pz, pz2;\n"
                   "  calc_quadz(2, 0, 1, 3, pz, pz2);\n"
                   "  if (gl_PrimitiveIDIn == 0) {\n"
                   "    emit_vertex(0, 3, pz);\n"
                   "    EndPrimitive();\n"
                   "    emit_vertex(1, 3, pz);\n"
                   "    EndPrimitive();\n"
                   "  }\n"
                   "  emit_vertex(2, 3, pz);\n"
                   "  EndPrimitive();\n"
                   "  emit_vertex(3, 3, pz2);\n"
                   "  EndPrimitive();\n";
        }
        break;
    case PRIM_TYPE_POLYGON:
        if (polygon_mode == POLY_MODE_FILL) {
            need_triz = true;
            layout_in = "layout(triangles) in;\n";
            layout_out = "layout(triangle_strip, max_vertices = 3) out;\n";
            body = "  mat3x4 pz = calc_triz(0, 1, 2);\n"
                   "  emit_vertex(0, 2, pz);\n"
                   "  emit_vertex(1, 2, pz);\n"
                   "  emit_vertex(2, 2, pz);\n"
                   "  EndPrimitive();\n";
        } else if (polygon_mode == POLY_MODE_LINE) {
            need_linez = true;
            layout_in = "layout(lines) in;\n";
            layout_out = "layout(line_strip, max_vertices = 2) out;\n";
            body = "  mat3x4 pz = calc_linez(0, 1);\n"
                   "  emit_vertex(0, 0, pz);\n"
                   "  emit_vertex(1, 1, pz);\n"
                   "  EndPrimitive();\n";
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
    MString *s = mstring_new();
    mstring_append_fmt(s, "#version %d\n\n", state->vulkan ? 450 : 400);
    mstring_append(s, layout_in);
    mstring_append(s, layout_out);
    mstring_append(s, "\n");
    pgraph_get_glsl_vtx_header(s, state->vulkan, state->smooth_shading, state->texture_perspective,
                               true, true, true);
    pgraph_get_glsl_vtx_header(s, state->vulkan, state->smooth_shading, state->texture_perspective,
                               false, false, false);

    if (state->smooth_shading) {
        mstring_append(
            s,
            "void emit_vertex(int index, int _unused, mat3x4 pz) {\n"
            "  gl_Position = gl_in[index].gl_Position;\n"
            "  gl_PointSize = gl_in[index].gl_PointSize;\n"
            "  vtxD0 = v_vtxD0[index];\n"
            "  vtxD1 = v_vtxD1[index];\n"
            "  vtxB0 = v_vtxB0[index];\n"
            "  vtxB1 = v_vtxB1[index];\n"
            "  vtxFog = v_vtxFog[index];\n"
            "  vtxT0 = v_vtxT0[index];\n"
            "  vtxT1 = v_vtxT1[index];\n"
            "  vtxT2 = v_vtxT2[index];\n"
            "  vtxT3 = v_vtxT3[index];\n"
            "  vtxPos = pz[1];\n"
            "  triDZ = (any(isnan(pz[0])) || any(isinf(pz[0]))) ? vec4(0.0) : pz[0];\n"
            "  triMZ = (isnan(pz[2].x) || isinf(pz[2].x)) ? 0.0 : pz[2].x;\n"
            "  EmitVertex();\n"
            "}\n");
    } else {
        mstring_append(
            s,
            "void emit_vertex(int index, int provoking_index, mat3x4 pz) {\n"
            "  gl_Position = gl_in[index].gl_Position;\n"
            "  gl_PointSize = gl_in[index].gl_PointSize;\n"
            "  vtxD0 = v_vtxD0[provoking_index];\n"
            "  vtxD1 = v_vtxD1[provoking_index];\n"
            "  vtxB0 = v_vtxB0[provoking_index];\n"
            "  vtxB1 = v_vtxB1[provoking_index];\n"
            "  vtxFog = v_vtxFog[index];\n"
            "  vtxT0 = v_vtxT0[index];\n"
            "  vtxT1 = v_vtxT1[index];\n"
            "  vtxT2 = v_vtxT2[index];\n"
            "  vtxT3 = v_vtxT3[index];\n"
            "  vtxPos = pz[1];\n"
            "  triDZ = (any(isnan(pz[0])) || any(isinf(pz[0]))) ? vec4(0.0) : pz[0];\n"
            "  triMZ = (isnan(pz[2].x) || isinf(pz[2].x)) ? 0.0 : pz[2].x;\n"
            "  EmitVertex();\n"
            "}\n");
    }

    dwfloat_append_base(s);
    dwfloat_append_det(s);

    if (state->z_perspective) {
        if (need_triz || need_quadz) {
            mstring_append(
                s,
                "mat3x4 calc_triz(int i0, int i1, int i2) {\n"
                "  mat2 m = mat2(v_vtxPos[i2].xy - v_vtxPos[i0].xy,\n"
                "                v_vtxPos[i1].xy - v_vtxPos[i0].xy);\n"
                "  vec4 b = vec4(dwf_div(two_sum(v_vtxPos[i0].w, -v_vtxPos[i2].w), vec2(0.0, v_vtxPos[i2].w)),\n"
                "                dwf_div(two_sum(v_vtxPos[i0].w, -v_vtxPos[i1].w), vec2(0.0, v_vtxPos[i1].w)));\n"
                // The following computes dzx and dzy same as
                // vec2 dz = b * inverse(m);
                // but with higher precision.
                "  vec2 det = dwf_det(m[0].x, m[1].y, m[1].x, m[0].y);\n"
                "  vec2 dzx = dwf_div(dwf_det(b.xy, m[1].y, b.zw, m[0].y), det);\n"
                "  vec2 dzy = dwf_div(dwf_det(b.zw, m[0].x, b.xy, m[1].x), det);\n"
                "  float triMZ = abs(max(abs(dzx.y), abs(dzy.y)) / v_vtxPos[i0].w);\n"
                "  mat3x4 pz = mat3x4(dzx, dzy, v_vtxPos[i0], triMZ, vec3(0.0));\n"
                "  return pz;\n"
                "}\n");
        }

        if (need_linez) {
            mstring_append(
                s,
                "mat3x4 calc_linez(int i0, int i1) {\n"
                "  vec2 delta_xy = v_vtxPos[i1].xy - v_vtxPos[i0].xy;\n"
                "  vec2 diw = dwf_div(two_sum(v_vtxPos[i0].w, -v_vtxPos[i1].w), vec2(0.0, v_vtxPos[i1].w));\n"
                "  vec2 d2 = dwf_det(delta_xy.x, delta_xy.x, delta_xy.y, -delta_xy.y);\n"
                "  vec2 dzx = dwf_div(dwf_mul(diw, delta_xy.x), d2);\n"
                "  vec2 dzy = dwf_div(dwf_mul(diw, delta_xy.y), d2);\n"
                "  return mat3x4(dzx, dzy, v_vtxPos[i0], vec4(0.0));\n"
                "}\n");
        }
    } else {
        if (need_triz || need_quadz) {
            mstring_append(
                s,
                "mat3x4 calc_triz(int i0, int i1, int i2) {\n"
                "  mat2 m = mat2(v_vtxPos[i2].xy - v_vtxPos[i0].xy,\n"
                "                v_vtxPos[i1].xy - v_vtxPos[i0].xy);\n"
                "  vec4 b = vec4(two_sum(v_vtxPos[i2].z, -v_vtxPos[i0].z),\n"
                "                two_sum(v_vtxPos[i1].z, -v_vtxPos[i0].z));\n"
                // The following computes dzx and dzy same as
                // vec2 dz = b * inverse(m);
                // but with higher precision.
                "  vec2 det = dwf_det(m[0].x, m[1].y, m[1].x, m[0].y);\n"
                "  vec2 dzx = dwf_div(dwf_det(b.xy, m[1].y, b.zw, m[0].y), det);\n"
                "  vec2 dzy = dwf_div(dwf_det(b.zw, m[0].x, b.xy, m[1].x), det);\n"
                "  mat3x4 pz = mat3x4(dzx, dzy, v_vtxPos[i0], max(abs(dzx.y), abs(dzy.y)), vec3(0.0));\n"
                "  return pz;\n"
                "}\n");
        }

        if (need_linez) {
            mstring_append(
                s,
                "mat3x4 calc_linez(int i0, int i1) {\n"
                "  vec2 delta_xy = v_vtxPos[i1].xy - v_vtxPos[i0].xy;\n"
                "  vec2 delta_z = two_sum(v_vtxPos[i1].z, -v_vtxPos[i0].z);\n"
                "  vec2 d2 = dwf_det(delta_xy.x, delta_xy.x, delta_xy.y, -delta_xy.y);\n"
                "  vec2 dzx = dwf_div(dwf_mul(delta_z, delta_xy.x), d2);\n"
                "  vec2 dzy = dwf_div(dwf_mul(delta_z, delta_xy.y), d2);\n"
                "  return mat3x4(dzx, dzy, v_vtxPos[i0], vec4(0.0));\n"
                "}\n");
        }
    }

    if (need_quadz) {
        mstring_append(
            s,
            "void calc_quadz(int i0, int i1, int i2, int i3, out mat3x4 triz1, out mat3x4 triz2) {\n"
            "  mat3x4 pz = calc_triz(i0, i1, i2);\n"
            "  mat3x4 pz2 = calc_triz(i0, i2, i3);\n"
            "  triz1 = (any(isnan(pz[0])) || any(isinf(pz[0]))) ? pz2 : pz;\n"
            "  triz2 = (any(isnan(pz2[0])) || any(isinf(pz2[0]))) ? pz : pz2;\n"
            "}\n");
    }

    mstring_append(s, "\n"
                      "void main() {\n");
    mstring_append(s, body);
    mstring_append(s, "}\n");

    return s;
}
