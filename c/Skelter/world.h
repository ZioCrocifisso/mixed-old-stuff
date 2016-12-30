#ifndef WORLD_H_INCLUDED
#define WORLD_H_INCLUDED

#include <stdint.h>
#include "connection.h"

typedef struct mc_block_data {
	int8_t id;
	int8_t metadata : 4;
	int8_t block_light : 4;
	int8_t sky_light : 4;	
	int8_t add : 4;
} mc_block;

struct mc_chunk {
	struct mc_block_data blocks[16][16][16];
};

struct mc_column {
	int8_t biome; //Cambiare in enum.
	int x, z;
	char empty;
	struct mc_chunk chunks[16];
	struct mc_column *next;
};

extern struct mc_column *mc_alloc_column(mc *c, int x, int z);
extern int mc_load_column(mc *c, int x, int z, uint16_t primary_bitmap, uint16_t add_bitmap, char guc, int size, void *data);
extern void mc_unload_column(mc *c, int x, int z);

extern mc_block mc_get_block(mc *c, int x, int y, int z);

#endif // WORLD_H_INCLUDED
