#include <zlib.h>
#include <math.h>

#include "world.h"
#include "connection.h"

static struct mc_column *get_column(mc *c, int x, int z);
static struct mc_column *get_prev_column(mc *c, int x, int z);

struct mc_column *mc_alloc_column(mc *c, int x, int z)
{
	struct mc_column *column = malloc(sizeof(struct mc_column));
	struct mc_column *prev;
	
	if (!column)
		return NULL;
	
	column->next = NULL;
	column->empty = 1;
	column->x = x;
	column->z = z;
	
	if (c->columns) {
		for (prev = c->columns; prev->next; prev = prev->next) ;
		prev->next = column;		
	} else {
		c->columns = column;
	}
	
	return column;
}

int mc_load_column(mc *c, int x, int z, uint16_t primary_bitmap, uint16_t add_bitmap, char guc, int size, void *data)
{
	struct mc_column *column = get_column(c, x, z);
	z_stream zs;
	int i, j, k, err;
	const int byte_chunk_unit = 4096;
	const int nibble_chunk_unit = byte_chunk_unit / 2;
	const int column_chunk_unit = 256;
	const int min_chunk_size = byte_chunk_unit + (nibble_chunk_unit) * 3;
	int effective_chunk_size = min_chunk_size + (column_chunk_unit * guc);
	uint8_t chunk_buffer[min_chunk_size + column_chunk_unit];
	
	//if (guc)
	//	primary_bitmap = 0xFFFF;
	
	if (!primary_bitmap)
		return;
	
	if (!column) {
		if (!(column = mc_alloc_column(c, x, z))) {
			return;
		}
	}
	
	zs.zalloc = Z_NULL;
	zs.zfree = Z_NULL;
	zs.opaque = Z_NULL;
	zs.avail_in = 0;
	zs.next_in = Z_NULL;
	
	if ((err = inflateInit(&zs)) != Z_OK)
		return err;
	
	zs.avail_in = size;
	zs.next_in = data;
	
	for (i = 0; i < 16; i++) {
		if (primary_bitmap & pow(2, i) == 0) {
			memset(chunk_buffer, 0, effective_chunk_size);
		} else do {
			zs.avail_out = effective_chunk_size;
			zs.next_out = chunk_buffer;
			
			err = inflate(&zs, Z_NO_FLUSH);
			
			switch (err) {
				case Z_STREAM_ERROR:
					return err;
					
				case Z_NEED_DICT:
				case Z_DATA_ERROR:
				case Z_MEM_ERROR:
					inflateEnd(&zs);
					return err;
			}			
		} while (zs.avail_out != 0 && zs.avail_in);
		
		#define brodello column->chunks[i].blocks[j & 0x0F][j >> 8][(j & 0xF0) >> 4]
		
		#define mdval(offset) (j % 2 == 0 ? chunk_buffer[j + offset] >> 4 : chunk_buffer[j + offset] & 0xF)
		
		for (j = 0; j < byte_chunk_unit; j++) {
			brodello.id = chunk_buffer[j];
		}
		
		for (j = 0; j < nibble_chunk_unit * 2; j++) {
			brodello.metadata = mdval(byte_chunk_unit);
		}
		
		for (j = 0; j < nibble_chunk_unit * 2; j++) {
			brodello.metadata = mdval(byte_chunk_unit + nibble_chunk_unit);
		}
		
		for (j = 0; j < nibble_chunk_unit * 2; j++) {
			brodello.metadata = mdval(byte_chunk_unit + (nibble_chunk_unit * 2));
		}
		
		guc = guc ? guc : guc;
		/*if (guc) {
			for (j = 0; j < column_chunk_unit; j++) {
				
			}
		}*/
	}
	
	inflateEnd(&zs);
	
	column->empty = 0;
	return err == Z_STREAM_END ? Z_OK : Z_DATA_ERROR;
}

void mc_unload_column(mc *c, int x, int z)
{
	struct mc_column *prev_column = get_prev_column(c, x, z);
	struct mc_column *column;
	
	if (prev_column) {
		column = prev_column->next;
		prev_column->next = NULL;
	} else if (c->columns) {
		column = c->columns;
		c->columns = NULL;
	} else {
		return;
	}
	
	free(column);
}

mc_block mc_get_block(mc *c, int x, int y, int z)
{
	mc_block block = { 0, 0, 0, 0, 0 };
	div_t xd = div(x, 16), yd = div(y, 16), zd = div(z, 16);
	struct mc_column *column = get_column(c, xd.quot, zd.quot);
	
	if (column && !column->empty) {
		return column->chunks[yd.quot].blocks[xd.rem][yd.rem][zd.rem];
	}
	
	return block;
}

struct mc_column *get_prev_column(mc *c, int x, int z)
{
	struct mc_column *curr;
	
	for (curr = c->columns; curr && curr->next; curr = curr->next) {
		if (curr->next->x == x && curr->next->z == z)
			return curr;
	}
	
	return NULL;
}

struct mc_column *get_column(mc *c, int x, int z)
{
	struct mc_column *curr;
	
	for (curr = c->columns; curr; curr = curr->next) {
		if (curr->x == x && curr->z == z)
			return curr;
	}
	
	return NULL;
}
