#ifndef EVENTS_H_INCLUDED
#define EVENTS_H_INCLUDED

#include <wchar.h>
#include "connection.h"
#include "packets.h"

enum mc_event_type {
	ev_chat_message,
	ev_login,
	ev_disconnect,
	ev_update,
	ev_health,
	ev_death
};

struct mc_event_chat_message {
	int type;
	wchar_t *message;
};

struct mc_event_health {
	int type;
	uint16_t health;
	uint16_t food;
	float saturation;
};

extern struct mc_event *mc_next_event(mc *client);

#endif // EVENTS_H_INCLUDED
