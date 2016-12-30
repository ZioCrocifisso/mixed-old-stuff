#ifndef BLOCK_H
#define BLOCK_H

#include <stdint.h>

typedef struct { uint32_t x, y, z; } mc_pos;

typedef struct {
	uint8_t data[2];
} mc_block_data;

typedef struct {
	mc_block_data data;
	mc_pos poss;
} mc_block;

#endif
