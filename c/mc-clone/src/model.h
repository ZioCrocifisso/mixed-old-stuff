#ifndef MODEL_H
#define MODEL_H

#include "vector.h"

typedef struct {
	GLuint vertex_buffer;
	size_t vertex_count;
} mc_model;

extern int mc_model_init(mc_model *model);
extern int mc_model_set_vertices(mc_model *model, int count, mc_vec3 *vertices);
extern void mc_model_free(mc_model *model);

#endif
