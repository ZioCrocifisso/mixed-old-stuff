#ifndef GRAPHICS_H
#define GRAPHICS_H

#include <GL/gl.h>

#include "model.h"
#include "vector.h"

typedef struct {
	GLuint program;
} mc_gc;

extern int mc_graphics_init(mc_gc *context);
extern int mc_graphics_shaders(mc_gc *context, char *vertex, char *fragment);
extern int mc_graphics_clear(mc_gc *context);
extern int mc_graphics_draw(mc_gc *context, mc_model *model);
extern int mc_graphics_destroy(mc_gc *context);

#endif
