#ifndef CHUNK_H
#define CHUNK_H

#include "block.h"
#include "graphics.h"

typedef mc_pos mc_chunkpos;

typedef struct {
	mc_block_data blocks[16][16][16];
	mc_model model;
} mc_chunk;

extern int mc_chunk_set_block(mc_chunk *chunk, mc_pos pos, mc_block *block);
extern int mc_chunk_get_block(mc_chunk *chunk, mc_pos pos, mc_block *block);
extern mc_block_data *mc_chunk_block_data(mc_chunk *chunk, mc_pos pos);

extern mc_chunkpos mc_chunk_getpos(mc_pos blockpos);

extern int mc_chunk_model_create(mc_chunk *chunk);
extern int mc_chunk_model_update(mc_chunk *chunk);
extern int mc_chunk_model_draw(mc_chunk *chunk);
extern void mc_chunk_model_free(mc_chunk *chunk);

#endif
