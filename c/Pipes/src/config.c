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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"

typedef struct node {
	struct node *next;
	unsigned char keylen, valuelen;
} node;

node first_node = { NULL, 0, 0 };
char *last_key = NULL;
node *last_node = NULL;

bool config_load(char *fname)
{
#define err() printf("Error in config %s at position %d.\r\n", fname, ftell(file))

	FILE *file;
	bool is_value = 0;
	node *n = &first_node;
	char c, key[256], value[1024];
	unsigned char keylen = 0, valuelen = 0;

	if (!fopen(fname, "r"))
		return false;

	while ((c = fgetc(file)) != EOF) {
		if (is_value) {
			if (c == '\n' || c == ';') {
				n->next = malloc(sizeof(struct node) + (keylen + valuelen) * sizeof(char) + 1);
				n = n->next;
				n->keylen = keylen;
				n->valuelen = valuelen;
				n->next = NULL;
				memcpy((void *) n->next + sizeof(struct node), (void *) key, keylen);
				value[valuelen] = '\0';
				memcpy((void *) n->next + sizeof(struct node) + keylen, (void *) value, valuelen);
				valuelen = 0;

				if (c == '\n') {
					keylen = 0;
					is_value = false;
				}
			} else if (valuelen < 1024)
				value[valuelen++] = c;
			else {
				err();
				return false;
			}
		} else {
			if (c == '=')
				is_value = true;
			else if (keylen < 256)
				key[keylen++] = c;
			else {
				err();
				return false;
			}
		}
	}
}

char *config_get_str(char *key)
{
	bool using_last = false;
	unsigned char keylen = strlen(key);
	node *n;
	char *nodekey, *nodevalue;

	if (!last_key || !key)
		return NULL;

	if ((using_last = (strcmp(last_key, key) == 0)))
		n = last_node;
	else
		n = first_node.next;

	while (n) {
		nodekey = (void *) (n + sizeof(struct node));
		nodevalue = (void *) (n + sizeof(struct node) + n->keylen);

		if (memcmp(key, nodekey, n->keylen < keylen ? n->keylen : keylen) == 0) {
			last_key = nodekey;
			last_node = n;
			return nodevalue;
		}
	}

	if (using_last) {
		last_key = NULL;
		last_node = NULL;
		return config_get_str(key);
	} else
		return NULL;
}

int config_get_num(char *key)
{
	char *str = config_get_str(key);
	
	if (str)
		return atoi(str);
	else
		return 0;
}

void config_free(void)
{
	node *current = first_node.next;
	node *next;

	while (current) {
		next = current->next;
		free(current);
		current = next;
	}
}
