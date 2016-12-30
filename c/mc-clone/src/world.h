#ifndef WORLD_H
#define WORLD_H

#include "block.h"
#include "chunk.h"

typedef struct {
	mc_chunkpos pos;
	mc_chunk *chunks[16][16][16];
} mc_bigchunk;

typedef struct {
	int bigchunkn;
	mc_bigchunk *bigchunks;
	unsigned int seed;
} mc_world;

extern int mc_world_init(mc_world *world, unsigned int seed);
extern int mc_world_generate(mc_world *world, int chunkn, mc_chunkpos *chunks);
extern int mc_world_regenerate(mc_world *world, int chunkn, mc_chunkpos *chunks);
extern mc_chunk *mc_world_chunk(mc_world *world, mc_chunkpos pos);
extern int mc_world_get_block(mc_world *world, mc_pos pos, mc_block *block);
extern int mc_world_set_block(mc_world *world, mc_pos pos, mc_block *block);
extern mc_block_data *mc_world_block_data(mc_world *world, mc_pos pos);
extern unsigned char mc_world_block_type(mc_world *world, mc_pos pos);
extern int mc_world_is_air(mc_world *world, mc_pos pos);
extern int mc_world_draw(mc_world *world, mc_pos start, unsigned int render_distance);
extern void mc_world_free(mc_world *world);

#endif
