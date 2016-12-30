#include "model.h"

int mc_model_init(mc_model *model)
{
	if (!model) {
		return 0;
	}

	glGenBuffers(1, &model->vertex_buffer);
	glBindBuffer(GL_ARRAY_BUFFER, model->vertex_buffer);

	model->vertex_count = 0;

	return 1;
}

int mc_model_set_vertices(mc_model *model, int count, mc_vec3 *vertices)
{
	if (!model) {
		return 0;
	}

	glBindBuffer(GL_ARRAY_BUFFER, model->vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER, count * 12, vertices, GL_DYNAMIC_DRAW);

	return 1;
}

void mc_model_free(mc_model *model)
{
	glDeleteBuffers(1, &model->vertex_buffer);
}
