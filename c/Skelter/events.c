#include <math.h>
#include <zlib.h>

#include "connection.h"
#include "events.h"
#include "packets.h"
#include "player.h"
#include "world.h"

static int hupdate(mc *c);
static int hdisconnect(mc *c, struct mc_packet rp);

static int add_event(mc *c, struct mc_event *e)
{
	if (c->cevents < 256) {
		c->events[c->cevents++] = e;
		return 1;
	} else {
		puts("event limit");
		return 0;
	}
}

extern struct mc_event *mc_next_event(mc *c)
{
	struct mc_packet p;

	init:
	if (c->cevents > 0) {
		return c->events[--c->cevents];
	} else {
		if (mc_packet_recv(c) == 0) {
			hdisconnect(c, p);
		} else if (c->spawn) {
			hupdate(c);
		}
		
		goto init;
	}
}

static int hdisconnect(mc *c, struct mc_packet rp) {
	struct mc_event *e;
	
	e = malloc(sizeof(struct mc_event));
	e->type = ev_disconnect;
	
	add_event(c, e);
}

static int hupdate(mc *c) {
	struct mc_packet sp;
	struct mc_event *e;
	
	e = malloc(sizeof(struct mc_event));
	e->type = ev_update;
	
	add_event(c, e);
	
	if (c->moving && c->looking)
		mc_packet_new(&sp, MCPACKET_PLAYER_POSLOOK);
	else if (c->moving)
		mc_packet_new(&sp, MCPACKET_PLAYER_POSITION);
	else if (c->looking)
		mc_packet_new(&sp, MCPACKET_PLAYER_LOOK);
	else
		mc_packet_new(&sp, MCPACKET_PLAYER_PLAYER);

	if (c->moving) {
		mc_packet_set(&sp, "x", &c->x, sizeof(double));
		mc_packet_set(&sp, "y", &c->y, sizeof(double));
		mc_packet_set(&sp, "z", &c->z, sizeof(double));
		mc_packet_set(&sp, "stance", &c->stance, sizeof(double));
	}
	
	if (c->looking) {
		mc_packet_set(&sp, "yaw", &c->yaw, sizeof(float));
		mc_packet_set(&sp, "pitch", &c->pitch, sizeof(float));
	}
	
	mc_packet_set(&sp, "on-ground", &c->on_ground, sizeof(int8_t));
	
	c->moving = 0;
	c->looking = 0;
	
	/*if (!c->flying && (!c->on_ground || 
		mc_get_block(c, (int) floor(c->x), ((int) floor(c->y)) - 1, (int) floor(c->z)).id == 0)) {
		mc_goto(c, c->x, c->y - 0.5, c->z);
	}*/

	mc_packet_send(c, sp);
}

static int hkeepalive(mc *c, struct mc_packet rp)
{
	struct mc_packet sp;
	void *data;
	int size;

	mc_packet_new(&sp, MCPACKET_KEEP_ALIVE);
	size = mc_packet_get(&rp, "keep-alive-id", &data, NULL);
	mc_packet_set(&sp, "keep-alive-id", data, size);
	return mc_packet_send(c, sp);
}

static int hhandshake(mc *c, struct mc_packet rp)
{
	struct mc_packet sp;
	int32_t pv = 29;

	mc_packet_new(&sp, MCPACKET_LOGIN);
	mc_packet_set(&sp, "protocol-version", &pv, sizeof(int32_t));
	mc_packet_set(&sp, "username", c->username, wcslen(c->username) * sizeof(wchar_t));
	return mc_packet_send(c, sp);
}

static int hlogin(mc *c, struct mc_packet rp)
{
	struct mc_event *e = malloc(sizeof(struct mc_event));
	struct mc_packet p;
	uint8_t true = 1;

	c->login = 1;
	puts("Login");

	mc_packet_get(&rp, "eid", NULL, &c->player_id);
	mc_packet_get(&rp, "server-mode", NULL, &c->gamemode);
	mc_packet_get(&rp, "dimension", NULL, &c->dimension);
	mc_packet_get(&rp, "difficulty", NULL, &c->difficulty);

	/*mc_packet_new(&p, MCPACKET_PLAYER_PLAYER);
	mc_packet_set(&p, "on-ground", &true, sizeof(uint8_t));
	mc_packet_send(c, p);*/

	if (e) {
		e->type = ev_login;
		add_event(c, e);
		return 1;
	}

	return 0;
}

static int hchatmessage(mc *c, struct mc_packet p)
{
	int size;
	size = mc_packet_get(&p, "message", NULL, NULL);
	struct mc_event_chat_message *e = malloc(sizeof(struct mc_event_chat_message) + size + sizeof(wchar_t));

	if (e) {
		e->type = ev_chat_message;
		e->message = ((void *) e) + sizeof(struct mc_event_chat_message);
		mc_packet_get(&p, "message", NULL, e->message);
		e->message[size / 4] = 0;
		add_event(c, (struct mc_event *) e);
		return 1;
	} else {
		return 0;
	}
}

static int hspawnpos(mc *c, struct mc_packet p)
{
	int x, y, z;

	mc_packet_get(&p, "x", NULL, &x);
	mc_packet_get(&p, "y", NULL, &y);
	mc_packet_get(&p, "z", NULL, &z);

	//c->spawn_x = (double) x;
	//c->spawn_y = (double) y;
	//c->spawn_z = (double) z;
}

static int hplayerposlook(mc *c, struct mc_packet rp)
{
	mc_packet_get(&rp, "x", NULL, &c->x);
	mc_packet_get(&rp, "y", NULL, &c->y);
	mc_packet_get(&rp, "z", NULL, &c->z);
	mc_packet_get(&rp, "stance", NULL, &c->stance);
	mc_packet_get(&rp, "yaw", NULL, &c->yaw);
	mc_packet_get(&rp, "pitch", NULL, &c->pitch);
	mc_packet_get(&rp, "on-ground", NULL, &c->on_ground);
	
	if (!c->on_ground)
		puts("not on ground");
	
	c->moving = 1;
	c->looking = 1;
	c->spawn = 1;
	
	hupdate(c);
}

static int hplayerabilities(mc *c, struct mc_packet rp)
{
	struct mc_packet sp;
	int8_t true = 1, false = 0;
	
	mc_packet_new(&sp, MCPACKET_PLAYER_ABILITIES);
	
	mc_packet_set(&sp, "invulnerability", &false, sizeof(int8_t));
	mc_packet_set(&sp, "is-flying", &false, sizeof(int8_t));
	mc_packet_set(&sp, "can-fly", &false, sizeof(int8_t));
	mc_packet_set(&sp, "instant-destroy", &false, sizeof(int8_t));
	
	mc_packet_send(c, sp);
}

static int hhealthupdate(mc *c, struct mc_packet rp)
{
	struct mc_event *e;
	uint16_t health;
	
	mc_packet_get(&rp, "health", NULL, &health);
	
	if (health > 0) {
		e = malloc(sizeof(struct mc_event_health));
		e->type = ev_health;
		
		mc_packet_get(&rp, "food", NULL, &((struct mc_event_health *) e)->food);
		mc_packet_get(&rp, "food-saturation", NULL, &((struct mc_event_health *) e)->saturation);
	} else {
		e = malloc(sizeof(struct mc_event));
		e->type = ev_death;
	}
	
	add_event(c, e);
}

int hcolumnalloc(mc *c, struct mc_packet rp)
{
	int8_t mode;
	int32_t x, z;
	
	mc_packet_get(&rp, "mode", NULL, &mode);
	mc_packet_get(&rp, "x", NULL, &x);
	mc_packet_get(&rp, "z", NULL, &z);
	
	if (mode) {
		mc_alloc_column(c, x, z);
	} else {
		mc_unload_column(c, x, z);
	}
}

int hcolumnchunks(mc *c, struct mc_packet rp)
{
	int32_t x, z, size, err;
	uint16_t pbm, abm;
	int8_t guc;
	void *data;
	
	mc_packet_get(&rp, "x", NULL, &x);
	mc_packet_get(&rp, "z", NULL, &z);
	mc_packet_get(&rp, "guc", NULL, &guc);
	mc_packet_get(&rp, "pbm", NULL, &pbm);
	mc_packet_get(&rp, "abm", NULL, &abm);
	mc_packet_get(&rp, "compressed-size", NULL, &size);
	mc_packet_get(&rp, "compressed-data", &data, NULL);
	
	err = mc_load_column(c, x, z, pbm, abm, guc, size, data);
	if (err != Z_OK) {
		printf("Error %d in chunk %d, %d", err, x, z);
	}
}

mc_packet_handler mc_packet_handlers[256] = {
	[0 ... 255] = NULL,
	[MCPACKET_KEEP_ALIVE] = &hkeepalive,
	[MCPACKET_HANDSHAKE] = &hhandshake,
	[MCPACKET_LOGIN] = &hlogin,
	[MCPACKET_CHAT] = &hchatmessage,

	[MCPACKET_SPAWN_POSITION] = &hspawnpos,
	
	[MCPACKET_HEALTH_UPDATE] = &hhealthupdate,

	[MCPACKET_PLAYER_POSLOOK] = &hplayerposlook,
	
	[MCPACKET_COLUMN_ALLOC] = &hcolumnalloc,
	[MCPACKET_COLUMN_CHUNKS] = &hcolumnchunks,
	
	[MCPACKET_PLAYER_ABILITIES] = &hplayerabilities,
	
	[MCPACKET_DISCONNECT] = &hdisconnect
};
