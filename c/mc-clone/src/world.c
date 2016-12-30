#include <stdlib.h>

#include "world.h"

static int generate(mc_world *world, int chunkn, mc_chunkpos *chunks, int force);

static mc_bigchunk *bigchunk_create(mc_world *world, mc_chunkpos pos);
static mc_bigchunk *bigchunk_search(mc_world *world, mc_chunkpos pos);
static mc_chunk *bigchunk_chunk(mc_bigchunk *bc, mc_chunkpos pos);
static mc_chunk **bigchunk_chunkptr(mc_bigchunk *bc, mc_chunkpos pos);
static void bigchunk_free(mc_bigchunk *bc);

int mc_world_init(mc_world *world, unsigned int seed)
{
	if (!world) {
		return 0;
	}

	world->bigchunkn = 0;
	world->bigchunks = (mc_bigchunk *) 0;
	world->seed = seed;

	return 1;
}

int mc_world_generate(mc_world *world, int chunkn, mc_chunkpos *chunks)
{
	return generate(world, chunkn, chunks, 0);
}

int mc_world_regenerate(mc_world *world, int chunkn, mc_chunkpos *chunks)
{
	return generate(world, chunkn, chunks, 1);
}

mc_chunk *mc_world_chunk(mc_world *world, mc_chunkpos pos)
{
	int i;
	mc_bigchunk *bc;

	if (!world) {
		return (mc_chunk *) 0;
	}

	if ((bc = bigchunk_search(world, pos)) != (mc_bigchunk *) 0) {
		return bigchunk_chunk(bc, pos);
	}

	return (mc_chunk *) 0;
}

int mc_world_get_block(mc_world *world, mc_pos pos, mc_block *block)
{
	mc_chunk *chunk;

	if (!world || !block) {
		return 0;
	}

	chunk = mc_world_chunk(world, mc_chunk_getpos(pos));

	if (!chunk) {
		return 0;
	}

	return mc_chunk_get_block(chunk, pos, block);
}

int mc_world_set_block(mc_world *world, mc_pos pos, mc_block *block)
{
	mc_chunk *chunk;

	if (!world || !block) {
		return 0;
	}

	chunk = mc_world_chunk(world, mc_chunk_getpos(pos));

	if (!chunk) {
		return 0;
	}

	return mc_chunk_set_block(chunk, pos, block);
}

mc_block_data *mc_world_block_data(mc_world *world, mc_pos pos)
{
	mc_chunk *chunk;

	if (!world || !block) {
		return 0;
	}

	chunk = mc_world_chunk(world, mc_chunk_getpos(pos));

	if (!chunk) {
		return 0;
	}

	return mc_chunk_block_data(chunk, pos);
}

unsigned char mc_world_block_type(mc_world *world, mc_pos pos)
{
	mc_block_data *data = mc_world_block_data(world, pos);

	if (data) {
		return data.data[0];
	} else {
		return 0;
	}
}

int mc_world_is_air(mc_world *world, mc_pos pos)
{
	mc_block_data *data = mc_world_block_data(world, pos);

	if (data) {
		return data.data[0] == 0;
	} else {
		return 0;
	}
}

int mc_world_draw(mc_world *world, mc_pos start, unsigned int render_distance)
{
}

void mc_world_free(mc_world *world)
{
	int i;

	for (i = 0; i < world->bigchunkn; i++) {
		bigchunk_free(&world->bigchunks[i]);
	}

	free(world->bigchunks);
}

static int generate(mc_world *world, int chunkn, mc_chunkpos *chunks, int force)
{
	int i, generated = 0;
	mc_pos j;
	mc_bigchunk *bc;
	mc_chunk **chunkptr;

	if (!chunks || !world) {
		return 0;
	}

	for (i = 0; i < chunkn; i++) {
		bc = bigchunk_create(world, chunks[i]);

		if (!bc) {
			return 0;
		}

		chunkptr = bigchunk_chunkptr(bc, chunks[i]);

		if (*chunkptr && !force) {
			continue;
		} else {
			*chunkptr = (mc_chunk *) malloc(sizeof(mc_chunk));

			if (!*chunkptr) {
				return 0;
			}

			for (j.x = 0; j.x < 16; j.x++) {
				for (j.y = 0; j.y < 16; j.y++) {
					for (j.z = 0; j.z < 16; j.z++) {
						if (j.y == 0) {
							(*chunkptr)->blocks[j.x][j.y][j.z].data[0] = 1;
						} else {
							(*chunkptr)->blocks[j.x][j.y][j.z].data[0] = 0;
						}
					}
				}
			}

			generated++;
		}
	}

	return generated;
}

static mc_bigchunk *bigchunk_create(mc_world *world, mc_chunkpos pos)
{
	mc_bigchunk *bclist;
	int x, y, z;

	if ((bclist = bigchunk_search(world, pos)) != (mc_bigchunk *) 0) {
		return bclist;
	}

	bclist = (mc_bigchunk *) realloc(
					(void *) world->bigchunks,
					sizeof(mc_bigchunk) * (world->bigchunkn + 1)
	);

	if (!bclist) {
		return (mc_bigchunk *) 0;
	}

	bclist += world->bigchunkn++;
	bclist->pos.x = pos.x - (pos.x % 16);
	bclist->pos.y = pos.y - (pos.y % 16);
	bclist->pos.z = pos.z - (pos.z % 16);

	for (x = 0; x < 16; x++) {
		for (y = 0; y < 16; y++) {
			for (z = 0; z < 16; z++) {
				bclist->chunks[x][y][z] = (mc_chunk *) 0;
			}
		}
	}

	return (world->bigchunks = bclist);
}

static mc_bigchunk *bigchunk_search(mc_world *world, mc_chunkpos pos)
{
	mc_bigchunk *bc;
	int i;

	for (i = 0; i < world->bigchunkn; i++) {
		bc = &world->bigchunks[i];

		if (
			pos.x >= bc->pos.x && pos.x < bc->pos.x + 16 &&
			pos.z >= bc->pos.z && pos.z < bc->pos.z + 16
		) {
			return bc;
		}
	}

	return (mc_bigchunk *) 0;
}

static mc_chunk *bigchunk_chunk(mc_bigchunk *bc, mc_chunkpos pos)
{
	return bc->chunks[pos.x % 16][pos.y % 16][pos.z % 16];
}

static mc_chunk **bigchunk_chunkptr(mc_bigchunk *bc, mc_chunkpos pos)
{
	return &bc->chunks[pos.x % 16][pos.y % 16][pos.z % 16];
}

static void bigchunk_free(mc_bigchunk *bc)
{
	int x, y, z;

	for (x = 0; x < 16; x++) {
		for (y = 0; y < 16; y++) {
			for (z = 0; z < 16; z++) {
				if (bc->chunks[x][y][z]) {
					free(bc->chunks[x][y][z]);
				}
			}
		}
	}
}
