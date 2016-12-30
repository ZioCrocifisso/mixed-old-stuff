#ifndef COMMANDS_H_INCLUDED
#define COMMANDS_H_INCLUDED

#include <wchar.h>

#define MC_PLAYER_HEIGHT 0.4

struct mc_item_slot {
	int16_t id;
	uint8_t count;
	int16_t damage;
};

extern int mc_send_message(mc *client, wchar_t *message);

extern int mc_goto(mc *client, double x, double y, double z);
extern int mc_look(mc *client, float yaw, float pitch);
extern int mc_respawn(mc *client);

extern int mc_read_slot(uint8_t **bp, struct mc_item_slot *slot);

#endif // COMMANDS_H_INCLUDED
