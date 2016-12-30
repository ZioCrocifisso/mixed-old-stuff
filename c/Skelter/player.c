#include <wchar.h>
#include "connection.h"
#include "packets.h"

int mc_send_message(mc *client, wchar_t *message)
{
	struct mc_packet p;
	
	mc_packet_new(&p, MCPACKET_CHAT);
	mc_packet_set(&p, "message", message, wcslen(message) * sizeof(wchar_t));
	
	return mc_packet_send(client, p);
}

int mc_goto(mc *client, double x, double y, double z)
{
	if (!client->moving) {
		client->moving = 1;
	
		client->x = x;
		client->stance = (client->stance - client->y) + y;
		client->y = y;
		client->z = z;
		
		return 1;
	} else {
		return 0;
	}
}

int mc_look(mc *client, float yaw, float pitch)
{
	if (!client->looking) {
		client->looking = 1;
		
		client->yaw = yaw;
		client->pitch = pitch;
		
		return 1;
	} else {
		return 0;
	}
}

int mc_respawn(mc *client)
{
	struct mc_packet p;
	int zero = 0;
	
	mc_packet_new(&p, MCPACKET_RESPAWN);
	mc_packet_set(&p, "dimension", &zero, sizeof(int32_t));
	mc_packet_set(&p, "difficulty", &zero, sizeof(int8_t));
	mc_packet_set(&p, "creative-mode", &zero, sizeof(int8_t));
	mc_packet_set(&p, "world-height", &zero, sizeof(int16_t));
	mc_packet_set(&p, "level-type", L"", sizeof(wchar_t));
	
	mc_packet_send(client, p);
}
