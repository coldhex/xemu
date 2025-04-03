#ifndef HW_XBOX_NV2A_PGRAPH_GLSL_DWFLOAT_H
#define HW_XBOX_NV2A_PGRAPH_GLSL_DWFLOAT_H

#include "qemu/mstring.h"

void dwfloat_append_base(MString *s);
void dwfloat_append_floor(MString *s);
void dwfloat_append_det(MString *s);
void dwfloat_append_dot(MString *s);

#endif
