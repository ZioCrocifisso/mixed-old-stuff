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

#ifndef PIPES_H
#define PIPES_H

#include <stdbool.h>
#include <stdint.h>

#define PIPE_LINK_U 1
#define PIPE_LINK_D 2
#define PIPE_LINK_R 4
#define PIPE_LINK_L 8

#define PIPE_MAIN 1
#define PIPE_ACTIVE 2

struct pipe {
	unsigned char links : 4;
	unsigned char status : 4;
};

struct grid {
	struct pipe *pipes;
	unsigned int width, height;
};

extern bool pipe_create_grid(struct grid *grid, unsigned int width, unsigned int height);
extern void pipe_gen_grid(struct grid *grid, int64_t seed);
extern bool pipe_save_grid(struct grid *grid, char *fname);
extern bool pipe_load_grid(struct grid *grid, char *fname);
extern struct pipe pipe_get(struct grid *grid, unsigned int cx, unsigned int cy);
extern void pipe_rotate(struct grid *grid, unsigned int cx, unsigned int cy);
extern bool pipe_completed(struct grid *grid);
extern void pipe_destroy_grid(struct grid *grid);

#endif
