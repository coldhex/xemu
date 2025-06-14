/*
 * QEMU Geforce NV2A pixel shader translation
 *
 * Copyright (c) 2013 espes
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

#ifndef HW_NV2A_PSH_H
#define HW_NV2A_PSH_H

#include <stdint.h>
#include <stdbool.h>

enum PshAlphaFunc {
    ALPHA_FUNC_NEVER,
    ALPHA_FUNC_LESS,
    ALPHA_FUNC_EQUAL,
    ALPHA_FUNC_LEQUAL,
    ALPHA_FUNC_GREATER,
    ALPHA_FUNC_NOTEQUAL,
    ALPHA_FUNC_GEQUAL,
    ALPHA_FUNC_ALWAYS,
};

enum PshShadowDepthFunc {
    SHADOW_DEPTH_FUNC_NEVER,
    SHADOW_DEPTH_FUNC_LESS,
    SHADOW_DEPTH_FUNC_EQUAL,
    SHADOW_DEPTH_FUNC_LEQUAL,
    SHADOW_DEPTH_FUNC_GREATER,
    SHADOW_DEPTH_FUNC_NOTEQUAL,
    SHADOW_DEPTH_FUNC_GEQUAL,
    SHADOW_DEPTH_FUNC_ALWAYS,
};

enum ConvolutionFilter {
    CONVOLUTION_FILTER_DISABLED,
    CONVOLUTION_FILTER_QUINCUNX,
    CONVOLUTION_FILTER_GAUSSIAN,
};

enum PshDepthFormat {
    DEPTH_FORMAT_D24,
    DEPTH_FORMAT_D16,
    DEPTH_FORMAT_F24,
    DEPTH_FORMAT_F16,
};

/* These are the most significant nibble of NV097_SET_TEXTURE_FILTER
 * i.e. NV097_SET_TEXTURE_FILTER_ASIGNED, NV097_SET_TEXTURE_FILTER_RSIGNED,
 * NV097_SET_TEXTURE_FILTER_GSIGNED and NV097_SET_TEXTURE_FILTER_BSIGNED.
 */
#define TEX_CHANNEL_ASIGNED 1
#define TEX_CHANNEL_RSIGNED 2
#define TEX_CHANNEL_GSIGNED 4
#define TEX_CHANNEL_BSIGNED 8
#define TEX_CHANNEL_SIGNED_MASK 0xF

typedef struct PshState {
    bool vulkan;

    /* fragment shader - register combiner stuff */
    uint32_t combiner_control;
    uint32_t shader_stage_program;
    uint32_t other_stage_input;
    uint32_t final_inputs_0;
    uint32_t final_inputs_1;

    uint32_t rgb_inputs[8], rgb_outputs[8];
    uint32_t alpha_inputs[8], alpha_outputs[8];

    bool stipple;
    bool point_sprite;
    bool rect_tex[4];
    bool compare_mode[4][4];
    bool alphakill[4];
    bool biased_tex[4];
    uint8_t tex_color_format[4];
    uint8_t tex_channel_signs[4];
    enum ConvolutionFilter conv_tex[4];
    int dim_tex[4];

    float border_logical_size[4][3];
    float border_inv_real_size[4][3];

    bool shadow_map[4];
    enum PshShadowDepthFunc shadow_depth_func;

    bool alpha_test;
    enum PshAlphaFunc alpha_func;

    bool window_clip_exclusive;

    bool texture_perspective;
    bool smooth_shading;
    bool depth_clipping;
    bool z_perspective;

    enum PshDepthFormat depth_format;
} PshState;

#endif
