#ifndef PACKETS_H_INCLUDED
#define PACKETS_H_INCLUDED

#define MCPACKET_KEEP_ALIVE		 	0x00
#define MCPACKET_LOGIN				0x01
#define MCPACKET_HANDSHAKE			0x02
#define MCPACKET_CHAT				0x03

#define MCPACKET_SPAWN_POSITION	 	0x06

#define MCPACKET_HEALTH_UPDATE		0x08
#define MCPACKET_RESPAWN			0x09

#define MCPACKET_PLAYER_PLAYER		0x0A
#define MCPACKET_PLAYER_POSITION	0x0B
#define MCPACKET_PLAYER_LOOK		0x0C
#define MCPACKET_PLAYER_POSLOOK	 	0x0D

#define MCPACKET_COLUMN_ALLOC		0x32
#define MCPACKET_COLUMN_CHUNKS		0x33
#define MCPACKET_MULTI_BLOCK_CHANGE	0x34
#define MCPACKET_BLOCK_CHANGE		0x35

#define MCPACKET_PLAYER_ABILITIES	0xCA

#define MCPACKET_DISCONNECT			0xFF

#include <stdint.h>

enum mc_data_type {
	dt_bool,
	dt_byte,
	dt_ubyte,
	dt_short,
	dt_ushort,
	dt_int,
	dt_long,
	dt_float,
	dt_double,
	dt_string,
	dt_slot,
	dt_metadata,
	dt_chunk,
	dt_special_34,
	dt_special_3C,
	dt_special_68,
	dt_special_83,
	dt_special_FA
};

enum mc_packet_version {
	pv_fromserver,
	pv_fromclient,
	pv_neutral
};

struct mc_packet {
	uint8_t id;
	char cfields;
	enum mc_packet_version version;
	struct {
		char *name;
		enum mc_data_type dt;
		int size;
		void *value;
	} fields[32];
};

extern int mc_add_packet_type(int id, enum mc_packet_version version, int dcfields, ...);
extern int mc_add_packet_types(void);

#endif // PACKETS_H_INCLUDED
