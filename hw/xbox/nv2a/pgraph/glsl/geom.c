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
#include "geom.h"

//#define CUSTOM_FRUSTUM_CLIPPING

static
MString *pgraph_gen_geom_tri_glsl(enum ShaderPolygonMode polygon_front_mode,
                                  enum ShaderPolygonMode polygon_back_mode,
                                  enum ShaderPrimitiveMode primitive_mode,
                                  bool smooth_shading,
                                  bool vulkan)
{
    /* FIXME: Missing support for 2-sided-poly mode */
    assert(polygon_front_mode == polygon_back_mode);
    enum ShaderPolygonMode polygon_mode = polygon_front_mode;
    assert(polygon_mode == POLY_MODE_FILL);

    /* Handle LINE and FILL mode */
    const char *layout_in = NULL;
    const char *layout_out = NULL;
    const char *body = NULL;
    switch (primitive_mode) {
    case PRIM_TYPE_TRIANGLES:
        layout_in = "layout(triangles) in;\n";
        layout_out = "layout(triangle_strip, max_vertices = 3) out;\n";
        body = "  emit_tri(0, 1, 2, 0);\n";
        break;
    case PRIM_TYPE_TRIANGLE_STRIP:
        layout_in = "layout(triangles) in;\n";
        layout_out = "layout(triangle_strip, max_vertices = 3) out;\n";
        body = "  emit_tri(0, 1, 2, 0);\n";
        break;
    case PRIM_TYPE_TRIANGLE_FAN:
        layout_in = "layout(triangles) in;\n";
        layout_out = "layout(triangle_strip, max_vertices = 3) out;\n";
        body = "  emit_tri(0, 1, 2, 0);\n";
        break;
    case PRIM_TYPE_QUADS:
        layout_in = "layout(lines_adjacency) in;\n";
        layout_out = "layout(triangle_strip, max_vertices = 6) out;\n";
        body = "  emit_tri(3, 0, 2, 3);\n"
               "  emit_tri(2, 0, 1, 3);\n";
        break;
    case PRIM_TYPE_QUAD_STRIP:
        layout_in = "layout(lines_adjacency) in;\n";
        layout_out = "layout(triangle_strip, max_vertices = 6) out;\n";
        body = "  if ((gl_PrimitiveIDIn & 1) != 0) { return; }\n"
               "  emit_tri(0, 1, 2, 3);\n"
               "  emit_tri(2, 1, 3, 3);\n";
        break;
    case PRIM_TYPE_POLYGON:
        layout_in = "layout(triangles) in;\n";
        layout_out = "layout(triangle_strip, max_vertices = 3) out;\n";
        body = "  emit_tri(0, 1, 2, 2);\n";
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
    mstring_append_fmt(s, "#version %d\n\n", vulkan ? 450 : 400);
    mstring_append(s, layout_in);
    mstring_append(s, layout_out);
    mstring_append(s, "\n");
    pgraph_get_glsl_vtx_header(s, vulkan, smooth_shading, true, true, true, true);
    pgraph_get_glsl_vtx_header(s, vulkan, smooth_shading, false, false, false, true);

    if (smooth_shading) {
        mstring_append(s,
                       "void emit_vertex(int index, int _unused) {\n"
                       "  gl_Position = gl_in[index].gl_Position;\n"
                       "  gl_PointSize = gl_in[index].gl_PointSize;\n"
                       "  EmitVertex();\n"
                       "}\n"
                       "\n"
                       "void set_tri_vars(int dst, int index) {\n"
                       "  tri_inv_w[dst] = v_vtx_inv_w[index];\n"
                       "  triD0[dst] = v_vtxD0[index];\n"
                       "  triD1[dst] = v_vtxD1[index];\n"
                       "  triB0[dst] = v_vtxB0[index];\n"
                       "  triB1[dst] = v_vtxB1[index];\n"
                       "  triFog[dst] = v_vtxFog[index];\n"
                       "  triT0[dst] = v_vtxT0[index];\n"
                       "  triT1[dst] = v_vtxT1[index];\n"
                       "  triT2[dst] = v_vtxT2[index];\n"
                       "  triT3[dst] = v_vtxT3[index];\n"
                       "}\n");
    } else {
        mstring_append(s,
                       "void emit_vertex(int index, int provoking_index) {\n"
                       "  gl_Position = gl_in[index].gl_Position;\n"
                       "  gl_PointSize = gl_in[index].gl_PointSize;\n"
                       "  vtxD0 = v_vtxD0[provoking_index];\n"
                       "  vtxD1 = v_vtxD1[provoking_index];\n"
                       "  vtxB0 = v_vtxB0[provoking_index];\n"
                       "  vtxB1 = v_vtxB1[provoking_index];\n"
                       "  EmitVertex();\n"
                       "}\n"
                       "\n"
                       "void set_tri_vars(int dst, int index) {\n"
                       "  tri_inv_w[dst] = v_vtx_inv_w[index];\n"
                       "  triFog[dst] = v_vtxFog[index];\n"
                       "  triT0[dst] = v_vtxT0[index];\n"
                       "  triT1[dst] = v_vtxT1[index];\n"
                       "  triT2[dst] = v_vtxT2[index];\n"
                       "  triT3[dst] = v_vtxT3[index];\n"
                       "}\n");
    }

    mstring_append(s,
                   "void emit_tri_vertex(int i0, int i1, int i2, int index, int provoking_index) {\n"
                   "  set_tri_vars(0, i0);\n"
                   "  set_tri_vars(1, i1);\n"
                   "  set_tri_vars(2, i2);\n"
                   "  emit_vertex(index, provoking_index);\n"
                   "}\n"
                   "void emit_tri(int i0, int i1, int i2, int provoking_index) {\n"
                   "  mat3 m = mat3(gl_in[i0].gl_Position.xyw / gl_in[i0].gl_Position.w,\n"
                   "                gl_in[i1].gl_Position.xyw / gl_in[i1].gl_Position.w,\n"
                   "                gl_in[i2].gl_Position.xyw / gl_in[i2].gl_Position.w);\n"
                   "  mat3 im = inverse(m);\n"
                   "  coordinv = im;\n"
                   "  emit_tri_vertex(i0, i1, i2, i0, provoking_index);\n"
                   "  coordinv = im;\n"
                   "  emit_tri_vertex(i0, i1, i2, i1, provoking_index);\n"
                   "  coordinv = im;\n"
                   "  emit_tri_vertex(i0, i1, i2, i2, provoking_index);\n"
                   "  EndPrimitive();\n"
                   "}\n");

    mstring_append(s, "\n"
                      "void main() {\n");
    mstring_append(s, body);
    mstring_append(s, "}\n");

    return s;
}

#ifndef CUSTOM_FRUSTUM_CLIPPING

MString *pgraph_gen_geom_glsl(enum ShaderPolygonMode polygon_front_mode,
                              enum ShaderPolygonMode polygon_back_mode,
                              enum ShaderPrimitiveMode primitive_mode,
                              bool smooth_shading,
                              bool custom_tri_interpolation,
                              bool vulkan)
{
    if (custom_tri_interpolation) {
        return pgraph_gen_geom_tri_glsl(polygon_front_mode, polygon_back_mode,
                                        primitive_mode, smooth_shading, vulkan);
    }

    /* FIXME: Missing support for 2-sided-poly mode */
    assert(polygon_front_mode == polygon_back_mode);
    enum ShaderPolygonMode polygon_mode = polygon_front_mode;

    /* POINT mode shouldn't require any special work */
    if (polygon_mode == POLY_MODE_POINT) {
        return NULL;
    }

    /* Handle LINE and FILL mode */
    const char *layout_in = NULL;
    const char *layout_out = NULL;
    const char *body = NULL;
    switch (primitive_mode) {
    case PRIM_TYPE_POINTS: return NULL;
    case PRIM_TYPE_LINES: return NULL;
    case PRIM_TYPE_LINE_LOOP: return NULL;
    case PRIM_TYPE_LINE_STRIP: return NULL;
    case PRIM_TYPE_TRIANGLES:
        if (polygon_mode == POLY_MODE_FILL) { return NULL; }
        assert(polygon_mode == POLY_MODE_LINE);
        layout_in = "layout(triangles) in;\n";
        layout_out = "layout(line_strip, max_vertices = 4) out;\n";
        body = "  emit_vertex(0, 0);\n"
               "  emit_vertex(1, 0);\n"
               "  emit_vertex(2, 0);\n"
               "  emit_vertex(0, 0);\n"
               "  EndPrimitive();\n";
        break;
    case PRIM_TYPE_TRIANGLE_STRIP:
        if (polygon_mode == POLY_MODE_FILL) { return NULL; }
        assert(polygon_mode == POLY_MODE_LINE);
        layout_in = "layout(triangles) in;\n";
        layout_out = "layout(line_strip, max_vertices = 4) out;\n";
        /* Imagine a quad made of a tristrip, the comments tell you which
         * vertex we are using */
        body = "  if ((gl_PrimitiveIDIn & 1) == 0) {\n"
               "    if (gl_PrimitiveIDIn == 0) {\n"
               "      emit_vertex(0, 0);\n" /* bottom right */
               "    }\n"
               "    emit_vertex(1, 0);\n" /* top right */
               "    emit_vertex(2, 0);\n" /* bottom left */
               "    emit_vertex(0, 0);\n" /* bottom right */
               "  } else {\n"
               "    emit_vertex(2, 0);\n" /* bottom left */
               "    emit_vertex(1, 0);\n" /* top left */
               "    emit_vertex(0, 0);\n" /* top right */
               "  }\n"
               "  EndPrimitive();\n";
        break;
    case PRIM_TYPE_TRIANGLE_FAN:
        if (polygon_mode == POLY_MODE_FILL) { return NULL; }
        assert(polygon_mode == POLY_MODE_LINE);
        layout_in = "layout(triangles) in;\n";
        layout_out = "layout(line_strip, max_vertices = 4) out;\n";
        body = "  if (gl_PrimitiveIDIn == 0) {\n"
               "    emit_vertex(0, 0);\n"
               "  }\n"
               "  emit_vertex(1, 0);\n"
               "  emit_vertex(2, 0);\n"
               "  emit_vertex(0, 0);\n"
               "  EndPrimitive();\n";
        break;
    case PRIM_TYPE_QUADS:
        layout_in = "layout(lines_adjacency) in;\n";
        if (polygon_mode == POLY_MODE_LINE) {
            layout_out = "layout(line_strip, max_vertices = 5) out;\n";
            body = "  emit_vertex(0, 3);\n"
                   "  emit_vertex(1, 3);\n"
                   "  emit_vertex(2, 3);\n"
                   "  emit_vertex(3, 3);\n"
                   "  emit_vertex(0, 3);\n"
                   "  EndPrimitive();\n";
        } else if (polygon_mode == POLY_MODE_FILL) {
            layout_out = "layout(triangle_strip, max_vertices = 4) out;\n";
            body = "  emit_vertex(3, 3);\n"
                   "  emit_vertex(0, 3);\n"
                   "  emit_vertex(2, 3);\n"
                   "  emit_vertex(1, 3);\n"
                   "  EndPrimitive();\n";
        } else {
            assert(false);
            return NULL;
        }
        break;
    case PRIM_TYPE_QUAD_STRIP:
        layout_in = "layout(lines_adjacency) in;\n";
        if (polygon_mode == POLY_MODE_LINE) {
            layout_out = "layout(line_strip, max_vertices = 5) out;\n";
            body = "  if ((gl_PrimitiveIDIn & 1) != 0) { return; }\n"
                   "  if (gl_PrimitiveIDIn == 0) {\n"
                   "    emit_vertex(0, 3);\n"
                   "  }\n"
                   "  emit_vertex(1, 3);\n"
                   "  emit_vertex(3, 3);\n"
                   "  emit_vertex(2, 3);\n"
                   "  emit_vertex(0, 3);\n"
                   "  EndPrimitive();\n";
        } else if (polygon_mode == POLY_MODE_FILL) {
            layout_out = "layout(triangle_strip, max_vertices = 4) out;\n";
            body = "  if ((gl_PrimitiveIDIn & 1) != 0) { return; }\n"
                   "  emit_vertex(0, 3);\n"
                   "  emit_vertex(1, 3);\n"
                   "  emit_vertex(2, 3);\n"
                   "  emit_vertex(3, 3);\n"
                   "  EndPrimitive();\n";
        } else {
            assert(false);
            return NULL;
        }
        break;
    case PRIM_TYPE_POLYGON:
        if (polygon_mode == POLY_MODE_LINE) {
            return NULL;
        }
        if (polygon_mode == POLY_MODE_FILL) {
            if (smooth_shading) {
                return NULL;
            }
            layout_in = "layout(triangles) in;\n";
            layout_out = "layout(triangle_strip, max_vertices = 3) out;\n";
            body = "  emit_vertex(0, 2);\n"
                   "  emit_vertex(1, 2);\n"
                   "  emit_vertex(2, 2);\n"
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
    mstring_append_fmt(s, "#version %d\n\n", vulkan ? 450 : 400);
    mstring_append(s, layout_in);
    mstring_append(s, layout_out);
    mstring_append(s, "\n");
    pgraph_get_glsl_vtx_header(s, vulkan, smooth_shading, true, true, true, false);
    pgraph_get_glsl_vtx_header(s, vulkan, smooth_shading, false, false, false, false);

    if (smooth_shading) {
        mstring_append(s,
                       "void emit_vertex(int index, int _unused) {\n"
                       "  gl_Position = gl_in[index].gl_Position;\n"
                       "  gl_PointSize = gl_in[index].gl_PointSize;\n"
                       "  vtx_inv_w = v_vtx_inv_w[index];\n"
                       "  vtxD0 = v_vtxD0[index];\n"
                       "  vtxD1 = v_vtxD1[index];\n"
                       "  vtxB0 = v_vtxB0[index];\n"
                       "  vtxB1 = v_vtxB1[index];\n"
                       "  vtxFog = v_vtxFog[index];\n"
                       "  vtxT0 = v_vtxT0[index];\n"
                       "  vtxT1 = v_vtxT1[index];\n"
                       "  vtxT2 = v_vtxT2[index];\n"
                       "  vtxT3 = v_vtxT3[index];\n"
                       "  EmitVertex();\n"
                       "}\n");
    } else {
        mstring_append(s,
                       "void emit_vertex(int index, int provoking_index) {\n"
                       "  gl_Position = gl_in[index].gl_Position;\n"
                       "  gl_PointSize = gl_in[index].gl_PointSize;\n"
                       "  vtx_inv_w = v_vtx_inv_w[index];\n"
                       "  vtxD0 = v_vtxD0[provoking_index];\n"
                       "  vtxD1 = v_vtxD1[provoking_index];\n"
                       "  vtxB0 = v_vtxB0[provoking_index];\n"
                       "  vtxB1 = v_vtxB1[provoking_index];\n"
                       "  vtxFog = v_vtxFog[index];\n"
                       "  vtxT0 = v_vtxT0[index];\n"
                       "  vtxT1 = v_vtxT1[index];\n"
                       "  vtxT2 = v_vtxT2[index];\n"
                       "  vtxT3 = v_vtxT3[index];\n"
                       "  EmitVertex();\n"
                       "}\n");
    }

    mstring_append(s, "\n"
                      "void main() {\n");
    mstring_append(s, body);
    mstring_append(s, "}\n");

    return s;
}

#else // CUSTOM_FRUSTUM_CLIPPING

MString *pgraph_gen_geom_glsl(enum ShaderPolygonMode polygon_front_mode,
                              enum ShaderPolygonMode polygon_back_mode,
                              enum ShaderPrimitiveMode primitive_mode,
                              bool smooth_shading,
                              bool custom_tri_interpolation,
                              bool vulkan)
{
    assert(!custom_tri_interpolation);
    printf("CUSTOM_FRUSTUM_CLIPPING compiled in");

    /* FIXME: Missing support for 2-sided-poly mode */
    assert(polygon_front_mode == polygon_back_mode);
    enum ShaderPolygonMode polygon_mode = polygon_front_mode;

    /* POINT mode shouldn't require any special work */
    if (polygon_mode == POLY_MODE_POINT) {
        return NULL;
    }

    /* Handle LINE and FILL mode */
    const char *layout_in = NULL;
    const char *layout_out = NULL;
    const char *body = NULL;
    switch (primitive_mode) {
    case PRIM_TYPE_POINTS: return NULL;
    case PRIM_TYPE_LINES: return NULL;
    case PRIM_TYPE_LINE_LOOP: return NULL;
    case PRIM_TYPE_LINE_STRIP: return NULL;
    case PRIM_TYPE_TRIANGLES:
        if (polygon_mode == POLY_MODE_FILL) {
            layout_in = "layout(triangles) in;\n";
            layout_out = "layout(triangle_strip, max_vertices = MAX_CLIPPED_VERTICES) out;\n";
            body = "  clip_tri(0, 1, 2, 0);\n";
        } else {
            assert(polygon_mode == POLY_MODE_LINE);
            layout_in = "layout(triangles) in;\n";
            layout_out = "layout(line_strip, max_vertices = 4) out;\n";
            body = "  emit_vertex(0, 0);\n"
                   "  emit_vertex(1, 0);\n"
                   "  emit_vertex(2, 0);\n"
                   "  emit_vertex(0, 0);\n"
                   "  EndPrimitive();\n";
        }
        break;
    case PRIM_TYPE_TRIANGLE_STRIP:
        if (polygon_mode == POLY_MODE_FILL) {
            layout_in = "layout(triangles) in;\n";
            layout_out = "layout(triangle_strip, max_vertices = MAX_CLIPPED_VERTICES) out;\n";
            body = "  clip_tri(0, 1, 2, 0);\n";
        } else {
            assert(polygon_mode == POLY_MODE_LINE);
            layout_in = "layout(triangles) in;\n";
            layout_out = "layout(line_strip, max_vertices = 4) out;\n";
            /* Imagine a quad made of a tristrip, the comments tell you which
             * vertex we are using */
            body = "  if ((gl_PrimitiveIDIn & 1) == 0) {\n"
                   "    if (gl_PrimitiveIDIn == 0) {\n"
                   "      emit_vertex(0, 0);\n" /* bottom right */
                   "    }\n"
                   "    emit_vertex(1, 0);\n" /* top right */
                   "    emit_vertex(2, 0);\n" /* bottom left */
                   "    emit_vertex(0, 0);\n" /* bottom right */
                   "  } else {\n"
                   "    emit_vertex(2, 0);\n" /* bottom left */
                   "    emit_vertex(1, 0);\n" /* top left */
                   "    emit_vertex(0, 0);\n" /* top right */
                   "  }\n"
                   "  EndPrimitive();\n";
        }
        break;
    case PRIM_TYPE_TRIANGLE_FAN:
        if (polygon_mode == POLY_MODE_FILL) {
            layout_in = "layout(triangles) in;\n";
            layout_out = "layout(triangle_strip, max_vertices = MAX_CLIPPED_VERTICES) out;\n";
            body = "  clip_tri(0, 1, 2, 0);\n";
        } else {
            assert(polygon_mode == POLY_MODE_LINE);
            layout_in = "layout(triangles) in;\n";
            layout_out = "layout(line_strip, max_vertices = 4) out;\n";
            body = "  if (gl_PrimitiveIDIn == 0) {\n"
                   "    emit_vertex(0, 0);\n"
                   "  }\n"
                   "  emit_vertex(1, 0);\n"
                   "  emit_vertex(2, 0);\n"
                   "  emit_vertex(0, 0);\n"
                   "  EndPrimitive();\n";
        }
        break;
    case PRIM_TYPE_QUADS:
        layout_in = "layout(lines_adjacency) in;\n";
        if (polygon_mode == POLY_MODE_LINE) {
            layout_out = "layout(line_strip, max_vertices = 5) out;\n";
            body = "  emit_vertex(0, 3);\n"
                   "  emit_vertex(1, 3);\n"
                   "  emit_vertex(2, 3);\n"
                   "  emit_vertex(3, 3);\n"
                   "  emit_vertex(0, 3);\n"
                   "  EndPrimitive();\n";
        } else if (polygon_mode == POLY_MODE_FILL) {
            layout_out = "layout(triangle_strip, max_vertices = MAX_CLIPPED_VERTICES_2x) out;\n";
            body = "  clip_tri(3, 0, 2, 3);\n"
                   "  clip_tri(2, 0, 1, 3);\n";
        } else {
            assert(false);
            return NULL;
        }
        break;
    case PRIM_TYPE_QUAD_STRIP:
        layout_in = "layout(lines_adjacency) in;\n";
        if (polygon_mode == POLY_MODE_LINE) {
            layout_out = "layout(line_strip, max_vertices = 5) out;\n";
            body = "  if ((gl_PrimitiveIDIn & 1) != 0) { return; }\n"
                   "  if (gl_PrimitiveIDIn == 0) {\n"
                   "    emit_vertex(0, 3);\n"
                   "  }\n"
                   "  emit_vertex(1, 3);\n"
                   "  emit_vertex(3, 3);\n"
                   "  emit_vertex(2, 3);\n"
                   "  emit_vertex(0, 3);\n"
                   "  EndPrimitive();\n";
        } else if (polygon_mode == POLY_MODE_FILL) {
            layout_out = "layout(triangle_strip, max_vertices = MAX_CLIPPED_VERTICES_2x) out;\n";
            body = "  if ((gl_PrimitiveIDIn & 1) != 0) { return; }\n"
                   "  clip_tri(0, 1, 2, 3);\n"
                   "  clip_tri(2, 1, 3, 3);\n";
        } else {
            assert(false);
            return NULL;
        }
        break;
    case PRIM_TYPE_POLYGON:
        if (polygon_mode == POLY_MODE_LINE) {
            return NULL;
        }
        if (polygon_mode == POLY_MODE_FILL) {
            layout_in = "layout(triangles) in;\n";
            layout_out = "layout(triangle_strip, max_vertices = MAX_CLIPPED_VERTICES) out;\n";
            body = "  clip_tri(0, 1, 2, 2);\n";
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
    mstring_append_fmt(s, "#version %d\n\n", vulkan ? 450 : 400);
    mstring_append(s, "#define MAX_CLIPPED_VERTICES 8\n");
    mstring_append(s, "#define MAX_CLIPPED_VERTICES_2x 16\n");
    mstring_append(s, layout_in);
    mstring_append(s, layout_out);
    mstring_append(s, "\n");
    pgraph_get_glsl_vtx_header(s, vulkan, smooth_shading, true, true, true, false);
    pgraph_get_glsl_vtx_header(s, vulkan, smooth_shading, false, false, false, false);

    if (smooth_shading) {
        mstring_append(s,
                       "void emit_vertex(int index, int _unused) {\n"
                       "  gl_Position = gl_in[index].gl_Position;\n"
                       "  gl_PointSize = gl_in[index].gl_PointSize;\n"
                       "  vtx_inv_w = v_vtx_inv_w[index];\n"
                       "  vtxD0 = v_vtxD0[index];\n"
                       "  vtxD1 = v_vtxD1[index];\n"
                       "  vtxB0 = v_vtxB0[index];\n"
                       "  vtxB1 = v_vtxB1[index];\n"
                       "  vtxFog = v_vtxFog[index];\n"
                       "  vtxT0 = v_vtxT0[index];\n"
                       "  vtxT1 = v_vtxT1[index];\n"
                       "  vtxT2 = v_vtxT2[index];\n"
                       "  vtxT3 = v_vtxT3[index];\n"
                       "  EmitVertex();\n"
                       "}\n"
                       "\n"
                       "struct MyVertex {\n"
                       "  vec4 Position;\n"
                       "  float PointSize;\n"
                       "  float vtx_inv_w;\n"
                       "  vec4 vtxD0;\n"
                       "  vec4 vtxD1;\n"
                       "  vec4 vtxB0;\n"
                       "  vec4 vtxB1;\n"
                       "  float vtxFog;\n"
                       "  vec4 vtxT0;\n"
                       "  vec4 vtxT1;\n"
                       "  vec4 vtxT2;\n"
                       "  vec4 vtxT3;\n"
                       "};\n"
                       "\n"
                       "MyVertex v[MAX_CLIPPED_VERTICES * 2];\n"
                       "void emit_vertex(MyVertex v) {\n"
                       "  v.Position.w = max(v.Position.w, 5.421011e-20);\n"
                       "  gl_Position = v.Position;\n"
                       "  gl_PointSize = v.PointSize;\n"
                       "  vtx_inv_w = v.vtx_inv_w/v.Position.w;\n"
                       "  vtxD0 = v.vtxD0/v.Position.w;\n"
                       "  vtxD1 = v.vtxD1/v.Position.w;\n"
                       "  vtxB0 = v.vtxB0/v.Position.w;\n"
                       "  vtxB1 = v.vtxB1/v.Position.w;\n"
                       "  vtxFog = v.vtxFog/v.Position.w;\n"
                       "  vtxT0 = v.vtxT0/v.Position.w;\n"
                       "  vtxT1 = v.vtxT1/v.Position.w;\n"
                       "  vtxT2 = v.vtxT2/v.Position.w;\n"
                       "  vtxT3 = v.vtxT3/v.Position.w;\n"
                       "  EmitVertex();\n"
                       "}\n"
                       "\n"
                       "MyVertex create_my_vertex(int index) {\n"
                       "  MyVertex v;\n"
                       "  v.Position = gl_in[index].gl_Position;\n"
                       "  v.PointSize = gl_in[index].gl_PointSize;\n"
                       "  v.vtx_inv_w = v_vtx_inv_w[index]*gl_in[index].gl_Position.w;\n"
                       "  v.vtxD0 = v_vtxD0[index]*gl_in[index].gl_Position.w;\n"
                       "  v.vtxD1 = v_vtxD1[index]*gl_in[index].gl_Position.w;\n"
                       "  v.vtxB0 = v_vtxB0[index]*gl_in[index].gl_Position.w;\n"
                       "  v.vtxB1 = v_vtxB1[index]*gl_in[index].gl_Position.w;\n"
                       "  v.vtxFog = v_vtxFog[index]*gl_in[index].gl_Position.w;\n"
                       "  v.vtxT0 = v_vtxT0[index]*gl_in[index].gl_Position.w;\n"
                       "  v.vtxT1 = v_vtxT1[index]*gl_in[index].gl_Position.w;\n"
                       "  v.vtxT2 = v_vtxT2[index]*gl_in[index].gl_Position.w;\n"
                       "  v.vtxT3 = v_vtxT3[index]*gl_in[index].gl_Position.w;\n"
                       "  return v;\n"
                       "}\n"
                       "\n"
                       "MyVertex interp_vert(MyVertex v0, MyVertex v1, float t) {\n"
                       "  MyVertex r;\n"
                       "  r.Position = mix(v0.Position, v1.Position, t);\n"
                       "  r.PointSize = mix(v0.PointSize, v1.PointSize, t);\n"
                       "  r.vtx_inv_w = mix(v0.vtx_inv_w, v1.vtx_inv_w, t);\n"
                       "  r.vtxD0 = mix(v0.vtxD0, v1.vtxD0, t);\n"
                       "  r.vtxD1 = mix(v0.vtxD1, v1.vtxD1, t);\n"
                       "  r.vtxB0 = mix(v0.vtxB0, v1.vtxB0, t);\n"
                       "  r.vtxB1 = mix(v0.vtxB1, v1.vtxB1, t);\n"
                       "  r.vtxFog = mix(v0.vtxFog, v1.vtxFog, t);\n"
                       "  r.vtxT0 = mix(v0.vtxT0, v1.vtxT0, t);\n"
                       "  r.vtxT1 = mix(v0.vtxT1, v1.vtxT1, t);\n"
                       "  r.vtxT2 = mix(v0.vtxT2, v1.vtxT2, t);\n"
                       "  r.vtxT3 = mix(v0.vtxT3, v1.vtxT3, t);\n"
                       "  return r;\n"
                       "}\n"
                       /*
                       "void clip_at_plane(vec4 plane, int src_index, int end_index, inout int dst_index) {\n"
                       "  int si = end_index - 1;\n"
                       "  for (int i = src_index; i < end_index; i++) {\n"
                       "    float doti = dot(v[i].Position, plane);\n"
                       "    float dotsi = dot(v[si].Position, plane);\n"
                       "    if (doti >= 0.0) {\n"
                       "      if (dotsi < 0.0) {\n"
                       "        float t = max(dotsi, -doti)/(dotsi - doti);\n"
                       "        bool c = doti > -dotsi;\n"
                       "        v[dst_index++] = interp_vert(v[c ? si : i], v[c ? i : si], t);\n"
                       "      }\n"
                       "      v[dst_index++] = v[i];\n"
                       "    } else if (dotsi >= 0.0) {\n"
                       "      float t = min(dotsi, -doti)/(dotsi - doti);\n"
                       "      bool c = -doti > dotsi;\n"
                       "      v[dst_index++] = interp_vert(v[c ? si : i], v[c ? i : si], t);\n"
                       "    }\n"
                       "    si = i;\n"
                       "  }\n"
                       "}\n"
                       */
                       /*
                       "void clip_at_plane(vec4 plane, int src_index, int end_index, inout int dst_index) {\n"
                       "  int si = end_index - 1;\n"
                       "  for (int i = src_index; i < end_index; i++) {\n"
                       "    float doti = dot(v[i].Position, plane);\n"
                       "    float dotsi = dot(v[si].Position, plane);\n"
                       "    float t = min(abs(dotsi), abs(doti)) / (abs(dotsi) + abs(doti));\n"
                       "    bool eg = abs(doti) > abs(dotsi);\n"
                       "    v[dst_index] = interp_vert(v[eg ? si : i], v[eg ? i : si], t);\n"
                       "    bool cross = (doti >= 0.0 && dotsi < 0.0) || (doti < 0.0 && dotsi >= 0.0);\n"
                       "    dst_index = dst_index + (cross ? 1 : 0);\n"
                       "    v[dst_index] = v[i];\n"
                       "    dst_index = dst_index + (doti >= 0.0 ? 1 : 0);\n"
                       "    si = i;\n"
                       "  }\n"
                       "}\n"
                       */
                       /*
                       "void clip_at_near(int src_index, int end_index, inout int dst_index) {\n"
                       //"  const float wc = 5.421011e-20;\n"
                       "  const float wc = 16.0/16777216.0;\n"
                       "  int si = end_index - 1;\n"
                       "  for (int i = src_index; i < end_index; i++) {\n"
                       "    float doti = v[i].Position.w - wc;\n"
                       "    float dotsi = v[si].Position.w - wc;\n"
                       "    if (doti >= 0.0) {\n"
                       "      if (dotsi < 0.0) {\n"
                       "        float t02 = dotsi/(dotsi - doti);\n"
                       "        v[dst_index++] = interp_vert(v[si], v[i], t02);\n"
                       "      }\n"
                       "      v[dst_index++] = v[i];\n"
                       "    } else if (dotsi >= 0.0) {\n"
                       "      float t02 = dotsi/(dotsi - doti);\n"
                       "      v[dst_index++] = interp_vert(v[si], v[i], t02);\n"
                       "    }\n"
                       "    si = i;\n"
                       "  }\n"
                       "}\n"
                       */
                       "void clip_at_plane(vec4 plane, int src_index, int end_index, inout int dst_index) {\n"
                       "  int si = end_index - 1;\n"
                       "  for (int i = src_index; i < end_index; i++) {\n"
                       "    float doti = dot(v[i].Position, plane);\n"
                       "    float dotsi = dot(v[si].Position, plane);\n"
                       "    if ((doti >= 0.0 && dotsi < 0.0) || (doti < 0.0 && dotsi >= 0.0)) {\n"
                       "      float t = min(abs(dotsi), abs(doti)) / (abs(dotsi) + abs(doti));\n"
                       "      bool c = abs(doti) > abs(dotsi);\n"
                       "      v[dst_index++] = interp_vert(v[c ? si : i], v[c ? i : si], t);\n"
                       "    }\n"
                       "    if (doti >= 0.0) {\n"
                       "      v[dst_index++] = v[i];\n"
                       "    }\n"
                       "    si = i;\n"
                       "  }\n"
                       "}\n"
                       "\n"
                       "void clip_tri(int i0, int i1, int i2, int _unused) {\n"
                       "  v[0] = create_my_vertex(i0);\n"
                       "  v[1] = create_my_vertex(i1);\n"
                       "  v[2] = create_my_vertex(i2);\n"
                       "  int i = MAX_CLIPPED_VERTICES;\n"
                       //"  clip_at_near(0, 3, i);\n"
                       //"  int j = 0;\n"
                       //"  clip_at_plane(vec4(1.0, 0.0, 0.0, 1.0), MAX_CLIPPED_VERTICES, i, j);\n"
                       //"  i = MAX_CLIPPED_VERTICES;\n"
                       //"  clip_at_plane(vec4(-1.0, 0.0, 0.0, 1.0), 0, j, i);\n"
                       //"  j = 0;\n"
                       //"  clip_at_plane(vec4(0.0, 1.0, 0.0, 1.0), MAX_CLIPPED_VERTICES, i, j);\n"
                       //"  i = MAX_CLIPPED_VERTICES;\n"
                       //"  clip_at_plane(vec4(0.0, -1.0, 0.0, 1.0), 0, j, i);\n"
                       //"  j = i - MAX_CLIPPED_VERTICES;\n"
                       "  clip_at_plane(vec4(1.0, 0.0, 0.0, 1.0), 0, 3, i);\n"
                       "  int j = 0;\n"
                       "  clip_at_plane(vec4(-1.0, 0.0, 0.0, 1.0), MAX_CLIPPED_VERTICES, i, j);\n"
                       "  i = MAX_CLIPPED_VERTICES;\n"
                       "  clip_at_plane(vec4(0.0, 1.0, 0.0, 1.0), 0, j, i);\n"
                       "  j = 0;\n"
                       "  clip_at_plane(vec4(0.0, -1.0, 0.0, 1.0), MAX_CLIPPED_VERTICES, i, j);\n"
                       "  if (j > 2) {\n"
                       "    emit_vertex(v[0]);\n"
                       "    for (int k = 1; k < j; k++) {\n"
                       "      int l = (1 + k/2)*(k%2) + (j - k/2)*(1 - (k%2));\n"
                       "      emit_vertex(v[l]);\n"
                       "    }\n"
                       "    EndPrimitive();\n"
                       "  }\n"
                       "}\n");
    } else {
        mstring_append(s,
                       "void emit_vertex(int index, int provoking_index) {\n"
                       "  gl_Position = gl_in[index].gl_Position;\n"
                       "  gl_PointSize = gl_in[index].gl_PointSize;\n"
                       "  vtx_inv_w = v_vtx_inv_w[index];\n"
                       "  vtxD0 = v_vtxD0[provoking_index];\n"
                       "  vtxD1 = v_vtxD1[provoking_index];\n"
                       "  vtxB0 = v_vtxB0[provoking_index];\n"
                       "  vtxB1 = v_vtxB1[provoking_index];\n"
                       "  vtxFog = v_vtxFog[index];\n"
                       "  vtxT0 = v_vtxT0[index];\n"
                       "  vtxT1 = v_vtxT1[index];\n"
                       "  vtxT2 = v_vtxT2[index];\n"
                       "  vtxT3 = v_vtxT3[index];\n"
                       "  EmitVertex();\n"
                       "}\n"
                       "\n"
                       "void clip_tri(int i0, int i1, int i2, int provoking_index) {\n"
                       "  emit_vertex(i0, provoking_index);\n"
                       "  emit_vertex(i1, provoking_index);\n"
                       "  emit_vertex(i2, provoking_index);\n"
                       "  EmitPrimitive();\n"
                       "}\n");
    }

    mstring_append(s, "\n"
                      "void main() {\n");
    mstring_append(s, body);
    mstring_append(s, "}\n");

    return s;
}

#endif // CUSTOM_FRUSTUM_CLIPPING
