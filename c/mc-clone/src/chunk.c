#include <string.h>

#include "block.h"
#include "chunk.h"
#include "graphics.h"

int mc_chunk_set_block(mc_chunk *chunk, mc_pos pos, mc_block *block)
{
	if (chunk && block) {
		return memcpy(
				(void *) &chunk->blocks[pos.x % 16][pos.y % 16][pos.z % 16],
				(void *) &block->data,
				sizeof(mc_block_data)
		) != (void *) 0;
	}

	return 0;
}

int mc_chunk_get_block(mc_chunk *chunk, mc_pos pos, mc_block *block)
{
	if (chunk && block) {
		return memcpy(
				(void *) &block->data,
				(void *) &chunk->blocks[pos.x % 16][pos.y % 16][pos.z % 16],
				sizeof(mc_block_data)
		) != (void *) 0;
	}

	return 0;
}

mc_block_data *mc_chunk_block_data(mc_chunk *chunk, mc_pos pos)
{
	if (chunk && block) {
		return &chunk->blocks[pos.x % 16][pos.y % 16][pos.z % 16];
	} else {
		return (mc_block_data *) 0;
	}
}

mc_chunkpos mc_chunk_getpos(mc_pos coords)
{
	mc_chunkpos cp;

	cp.x = coords.x / 16;
	cp.y = coords.y / 16;
	cp.z = coords.z / 16;

	return cp;
}

int mc_chunk_model_create(mc_chunk *chunk)
{
	return mc_model_init(&chunk->model);
}

int mc_chunk_model_update(mc_chunk *chunk)
{
}

int mc_chunk_model_draw(mc_chunk *chunk)
{
	return mc_graphics_draw(context, &chunk->model);
}

void mc_chunk_model_free(mc_chunk *chunk)
{
	mc_model_free(&chunk->model);
}
