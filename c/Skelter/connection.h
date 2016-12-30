#ifndef CONNECTION_H_INCLUDED
#define CONNECTION_H_INCLUDED

#include <stdlib.h>
#include "packets.h"

struct mc_event {
	int type;
};

typedef struct {
	int socket;
	wchar_t *username;

	enum {
		disconnected,
		handshake_wait,
		login_wait,
		connected
	} connection_status;

	double x, y, z, stance;
	float yaw, pitch;
	int8_t on_ground;
	
	int moving, looking, flying;
	int login, spawn;

	int cevents;
	struct mc_event *events[256];

	int player_id;

	enum {
		survival,
		creative
	} gamemode;

	enum {
		nether = -1,
		overworld,
		end
	} dimension;

	enum {
		peaceful,
		easy,
		normal,
		hard
	} difficulty;
	
	struct mc_column *columns;
} mc;

typedef int (* mc_packet_handler)(mc *client, struct mc_packet packet);

extern int mc_connect(mc *client, char *hostname, unsigned short port, wchar_t *username);

extern int mc_packet_new(struct mc_packet *packet, uint8_t id);
extern int mc_packet_send(mc *client, struct mc_packet packet);
extern int mc_packet_recv(mc *client);
extern int mc_packet_set(struct mc_packet *packet, char *field, void *data, int size);
extern int mc_packet_get(struct mc_packet *packet, char *field, void **data, void *copy);

extern int mc_disconnect(mc *client);

extern mc_packet_handler mc_packet_handlers[256];

#endif // CONNECTION_H_INCLUDED
