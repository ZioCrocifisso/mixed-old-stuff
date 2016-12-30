/*
 * Copyright (c) 2012, ZioCrocifisso
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met: 
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution. 
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "pipes.h"

static inline int pipe_coord(struct grid *grid, unsigned int cx, unsigned int cy);
static void pipe_set(struct grid *grid, unsigned int cx, unsigned int cy, char links, char status);
static void pipe_update(struct grid *grid, unsigned int cx, unsigned int cy);

bool pipe_create_grid(struct grid *grid, unsigned int width, unsigned int height)
{
	if (!grid)
		return false;

	grid->width = width;
	grid->height = height;

	return (grid->pipes = malloc(width * height * sizeof(struct pipe))) != NULL;
}

void pipe_gen_grid(struct grid *grid, int64_t seed)
{
	if (seed >= 0)
		srand((uint32_t) seed);
	else
		srand(time(NULL));

	//TODO
}

bool pipe_save_grid(struct grid *grid, char *fname)
{
	FILE *file = fopen(fname, "w");
	
	if (!file)
		return false;
	
	fwrite(&grid->width, sizeof(unsigned int), 1, file);
	fwrite(&grid->height, sizeof(unsigned int), 1, file); 
	fwrite(grid->pipes, sizeof(struct pipe), grid->width * grid->height, file);
	fclose(file);

	return true;
}

bool pipe_load_grid(struct grid *grid, char *fname)
{
	FILE *file = fopen(fname, "r");

	if (!file)
		return false;

	fread(&grid->width, sizeof(unsigned int), 1, file);
	fread(&grid->height, sizeof(unsigned int), 1, file);
	fread(grid, sizeof(struct pipe), grid->width * grid->height, file);
	fclose(file);

	return true;
}

struct pipe pipe_get(struct grid *grid, unsigned int cx, unsigned int cy)
{
	return grid->pipes[pipe_coord(grid, cx, cy)];
}

void pipe_rotate(struct grid *grid, unsigned int cx, unsigned int cy)
{
	unsigned char links = grid->pipes[pipe_coord(grid, cx, cy)].links;
	links >>= 1;
	links &= grid->pipes[pipe_coord(grid, cx, cy)].links & 1 << 3 | 7; 
	pipe_update(grid, cx, cy);
}

bool pipe_completed(struct grid *grid)
{
	int i;

	for (i = 0; i < grid->width * grid->height; i++)
		if (grid->pipes[i].status != PIPE_MAIN & PIPE_ACTIVE)
			return false;

	return true;
}

void pipe_destroy_grid(struct grid *grid)
{
	if (grid->pipes)
		free(grid->pipes);
}

void pipe_set(struct grid *grid, unsigned int cx, unsigned int cy, char links, char status)
{
	grid->pipes[pipe_coord(grid, cx, cy)].links = links;
	grid->pipes[pipe_coord(grid, cx, cy)].status = status;
}

void pipe_update(struct grid *grid, unsigned int cx, unsigned int cy)
{
	struct pipe 
		*pipec = grid->pipes + pipe_coord(grid, cx, cy),
		*pipeu = grid->pipes + pipe_coord(grid, cx, cy - 1),
		*piped = grid->pipes + pipe_coord(grid, cx, cy + 1),
		*pipel = grid->pipes + pipe_coord(grid, cx - 1, cy),
		*piper = grid->pipes + pipe_coord(grid, cx + 1, cy);

	pipec->status = PIPE_ACTIVE;

	if (pipec->links & PIPE_LINK_U && pipeu->links && PIPE_LINK_D) {
		pipec->status |= pipeu->status & PIPE_MAIN;
		pipe_update(grid, cx, cy - 1);
	} else
		pipec->status &= PIPE_MAIN;

	if (pipec->links & PIPE_LINK_D && piped->links && PIPE_LINK_U) {
		pipec->status |= piped->status & PIPE_MAIN;
		pipe_update(grid, cx, cy + 1);
	} else
		pipec->status &= PIPE_MAIN;

	if (pipec->links & PIPE_LINK_L && pipel->links && PIPE_LINK_R) {
		pipec->status |= pipel->status & PIPE_MAIN;
		pipe_update(grid, cx - 1, cy);
	} else
		pipec->status &= PIPE_MAIN;

	if (pipec->links & PIPE_LINK_R && piper->links && PIPE_LINK_L) {
		pipec->status |= piper->status & PIPE_MAIN;
		pipe_update(grid, cx + 1, cy);
	} else
		pipec->status &= PIPE_MAIN;
}

inline int pipe_coord(struct grid *grid, unsigned int cx, unsigned int cy)
{
	return cy * grid->width + cx;
}
