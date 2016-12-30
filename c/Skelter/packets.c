#include "connection.h"

int mc_add_packet_types(void)
{
	mc_add_packet_type(0x00, pv_neutral, 2, "keep-alive-id", dt_int);
	mc_add_packet_type(0x01, pv_fromclient, 16,
			"protocol-version",dt_int,
			"username",		dt_string,
			"",				dt_string,
			"",				dt_int,
			"",				dt_int,
			"",				dt_byte,
			"",				dt_ubyte,
			"",				dt_ubyte
	);
	mc_add_packet_type(0x01, pv_fromserver, 16,
			"eid",			dt_int,
			"",				dt_string,
			"level-type",	dt_string,
			"server-mode",	dt_int,
			"dimension",	dt_int,
			"difficulty",	dt_byte,
			"",				dt_ubyte,
			"max-players",	dt_ubyte
	);
	mc_add_packet_type(0x02, pv_fromclient, 2, "user-host", dt_string);
	mc_add_packet_type(0x02, pv_fromserver, 2, "connection-hash", dt_string);
	mc_add_packet_type(0x03, pv_neutral, 2, "message", dt_string);
	mc_add_packet_type(0x04, pv_neutral, 2, "time", dt_long);
	mc_add_packet_type(0x05, pv_neutral, 8,
			"eid",			dt_int,
			"slot",			dt_short,
			"item-id",		dt_short,
			"damage",		dt_short
	);
	mc_add_packet_type(0x06, pv_neutral, 6, "x", dt_int, "y", dt_int, "z", dt_int);
	mc_add_packet_type(0x07, pv_neutral, 6, "user", dt_int, "target", dt_int, "mouse-button", dt_bool);
	mc_add_packet_type(0x08, pv_neutral, 6, "health", dt_short, "food", dt_short, "food-saturation", dt_float);
	mc_add_packet_type(0x09, pv_neutral, 10,
			"dimension",	dt_int,
			"difficulty",	dt_byte,
			"creative-mode",dt_byte,
			"world-height",	dt_short,
			"level-type",	dt_string
	);
	mc_add_packet_type(0x0A, pv_neutral, 2, "on-ground", dt_bool);
	mc_add_packet_type(0x0B, pv_neutral, 10,
			"x",			dt_double,
			"y",			dt_double,
			"stance",		dt_double,
			"z",			dt_double,
			"on-ground",	dt_bool
	);
	mc_add_packet_type(0x0C, pv_neutral, 6,
			"yaw",			dt_float,
			"pitch",		dt_float,
			"on-ground",	dt_bool
	);
	mc_add_packet_type(0x0D, pv_fromclient, 14,
			"x",			dt_double,
			"y",			dt_double,
			"stance",		dt_double,
			"z",			dt_double,
			"yaw",			dt_float,
			"pitch",		dt_float,
			"on-ground",	dt_bool
	);
	mc_add_packet_type(0x0D, pv_fromserver, 14,
			"x",			dt_double,
			"stance",		dt_double,
			"y",			dt_double,
			"z",			dt_double,
			"yaw",			dt_float,
			"pitch",		dt_float,
			"on-ground",	dt_bool
	);
	mc_add_packet_type(0x0E, pv_neutral, 10,
			"status",		dt_byte,
			"x",			dt_int,
			"y",			dt_byte,
			"z",			dt_int,
			"face",			dt_byte
	);
	mc_add_packet_type(0x0F, pv_neutral, 10,
			"x",			dt_int,
			"y",			dt_ubyte,
			"z",			dt_int,
			"direction",	dt_byte,
			"held_item",	dt_slot
	);
	mc_add_packet_type(0x10, pv_neutral, 2, "slot-id", dt_short);
	mc_add_packet_type(0x11, pv_neutral, 10,
			"eid",			dt_int,
			"",				dt_byte,
			"x",			dt_int,
			"y",			dt_byte,
			"z",			dt_int
	);
	mc_add_packet_type(0x12, pv_neutral, 4, "eid", dt_int, "animation", dt_byte);
	mc_add_packet_type(0x13, pv_neutral, 4, "eid", dt_int, "action-id", dt_byte);
	mc_add_packet_type(0x14, pv_neutral, 16,
			"eid",			dt_int,
			"player-name",	dt_string,
			"x",			dt_int,
			"y",			dt_int,
			"z",			dt_int,
			"yaw",			dt_byte,
			"pitch",		dt_byte,
			"current-item",	dt_short
	);
	mc_add_packet_type(0x15, pv_neutral, 20,
			"eid",			dt_int,
			"item",			dt_short,
			"count",		dt_byte,
			"damage+data",	dt_short,
			"x",			dt_int,
			"y",			dt_int,
			"z",			dt_int,
			"rotation",		dt_byte,
			"pitch",		dt_byte,
			"roll",			dt_byte
	);
	mc_add_packet_type(0x16, pv_neutral, 4, "collected", dt_int, "collector", dt_int);
	mc_add_packet_type(0x17, pv_neutral, 18,
			"eid",			dt_int,
			"type",			dt_byte,
			"x",			dt_int,
			"y",			dt_int,
			"z",			dt_int,
			"fireball-thrower",dt_int,
			"speed-x",		dt_short,
			"speed-y",		dt_short,
			"speed-z",		dt_short
	);
	mc_add_packet_type(0x18, pv_neutral, 18,
			"eid",			dt_int,
			"type",			dt_byte,
			"x",			dt_int,
			"y",			dt_int,
			"z",			dt_int,
			"yaw",			dt_byte,
			"pitch",		dt_byte,
			"head-yaw",		dt_byte,
			"metadata",		dt_metadata);
	mc_add_packet_type(0x19, pv_neutral, 12,
			"eid",			dt_int,
			"title",		dt_string,
			"x",			dt_int,
			"y",			dt_int,
			"z",			dt_int,
			"direction",	dt_int
	);
	mc_add_packet_type(0x1A, pv_neutral, 10,
			"eid",			dt_int,
			"x",			dt_int,
			"y",			dt_int,
			"z",			dt_int,
			"count",		dt_short
	);
	mc_add_packet_type(0x1C, pv_neutral, 8,
			"eid",			dt_int,
			"velocity-x",	dt_short,
			"velocity-y",	dt_short,
			"velocity-z",	dt_short
	);
	mc_add_packet_type(0x1D, pv_neutral, 2, "eid", dt_int);
	mc_add_packet_type(0x1E, pv_neutral, 2, "eid", dt_int);
	mc_add_packet_type(0x1F, pv_neutral, 8,
			"eid",			dt_int,
			"dx",			dt_byte,
			"dy",			dt_byte,
			"dz",			dt_byte
	);
	mc_add_packet_type(0x20, pv_neutral, 6, "eid", dt_int, "yaw", dt_byte, "pitch", dt_byte);
	mc_add_packet_type(0x21, pv_neutral, 12,
			"eid",			dt_int,
			"dx",			dt_byte,
			"dy",			dt_byte,
			"dz",			dt_byte,
			"yaw",			dt_byte,
			"pitch",		dt_byte
	);
	mc_add_packet_type(0x22, pv_neutral, 12,
			"eid",			dt_int,
			"x",			dt_int,
			"y",			dt_int,
			"z",			dt_int,
			"yaw",			dt_byte,
			"pitch",		dt_byte
	);
	mc_add_packet_type(0x23, pv_neutral, 4, "eid", dt_int, "head-yaw", dt_byte);
	mc_add_packet_type(0x26, pv_neutral, 4, "eid", dt_int, "status", dt_byte);
	mc_add_packet_type(0x27, pv_neutral, 4, "eid", dt_int, "vehicle-id", dt_int);
	mc_add_packet_type(0x28, pv_neutral, 4, "eid", dt_int, "metadata", dt_metadata);
	mc_add_packet_type(0x29, pv_neutral, 8,
			"eid",			dt_int,
			"effect-id",	dt_byte,
			"amplifier",	dt_byte,
			"duration",		dt_short
	);
	mc_add_packet_type(0x2A, pv_neutral, 4, "eid", dt_int, "effect-id", dt_byte);
	mc_add_packet_type(0x2B, pv_neutral, 6, "experience-bar", dt_float, "level", dt_short, "total-experience", dt_short);
	mc_add_packet_type(0x32, pv_neutral, 6, "x", dt_int, "z", dt_int, "mode", dt_bool);
	mc_add_packet_type(0x33, pv_neutral, 16,
			"x",			dt_int,
			"z",			dt_int,
			"guc",			dt_bool,
			"pbm",			dt_ushort,
			"abm",			dt_ushort,
			"compressed-size",dt_int,
			"",				dt_int,
			"compressed-data",dt_chunk
	);
	mc_add_packet_type(0x34, pv_neutral, 10,
			"chunk-x",		dt_int,
			"chunk-z",		dt_int,
			"record-count",	dt_short,
			"data-size",	dt_int,
			"data",			dt_special_34
	);
	mc_add_packet_type(0x35, pv_neutral, 10,
			"x",			dt_int,
			"y",			dt_byte,
			"z",			dt_int,
			"block-type",	dt_byte,
			"block-metadata",dt_byte
	);
	mc_add_packet_type(0x36, pv_neutral, 10,
			"x",			dt_int,
			"y",			dt_short,
			"z",			dt_int,
			"byte-1",		dt_byte,
			"byte-2",		dt_byte
	);
	mc_add_packet_type(0x3C, pv_neutral, 12,
			"x",			dt_double,
			"y",			dt_double,
			"z",			dt_double,
			"",				dt_float,
			"record-count",	dt_int,
			"records",		dt_special_3C
	);
	mc_add_packet_type(0x3D, pv_neutral, 10,
			"effect-id",	dt_int,
			"x",			dt_int,
			"y",			dt_byte,
			"z",			dt_int,
			"data",			dt_int
	);
	mc_add_packet_type(0x46, pv_neutral, 4, "reason", dt_byte, "game-mode", dt_byte);
	mc_add_packet_type(0x47, pv_neutral, 10,
			"eid",			dt_int,
			"",				dt_bool,
			"x",			dt_int,
			"y",			dt_int,
			"z",			dt_int
	);
	mc_add_packet_type(0x64, pv_neutral, 8,
			"window-id",	dt_byte,
			"inventory-type",dt_byte,
			"window-title",	dt_string,
			"slots",		dt_byte
	);
	mc_add_packet_type(0x65, pv_neutral, 2, "window-id", dt_byte);
	mc_add_packet_type(0x66, pv_neutral, 12,
			"window-id",	dt_byte,
			"slot",			dt_short,
			"right-click",	dt_byte,
			"action-number",dt_short,
			"shift",		dt_bool,
			"clicked-item",	dt_slot
	);
	mc_add_packet_type(0x67, pv_neutral, 6, "window-id", dt_byte, "slot", dt_short, "slot-data", dt_slot);
	mc_add_packet_type(0x68, pv_neutral, 6, "window-id", dt_byte, "count", dt_short, "slot-data", dt_special_68);
	mc_add_packet_type(0x69, pv_neutral, 6, "window-id", dt_byte, "property", dt_short, "value", dt_short);
	mc_add_packet_type(0x6A, pv_neutral, 6, "window-id", dt_byte, "action-number", dt_short, "accepted", dt_bool);
	mc_add_packet_type(0x6B, pv_neutral, 4, "slot", dt_short, "clicked-item", dt_slot);
	mc_add_packet_type(0x6C, pv_neutral, 4, "window-id", dt_byte, "enchantment", dt_byte);
	mc_add_packet_type(0x82, pv_neutral, 14,
			"x",			dt_int,
			"y",			dt_short,
			"z",			dt_int,
			"text1",		dt_string,
			"text2",		dt_string,
			"text3",		dt_string,
			"text4",		dt_string
	);
	mc_add_packet_type(0x83, pv_neutral, 8,
			"item-type",	dt_short,
			"item-id",		dt_short,
			"text-length",	dt_ubyte,
			"text",			dt_special_83
	);
	mc_add_packet_type(0x84, pv_neutral, 14,
			"x",			dt_int,
			"y",			dt_short,
			"z",			dt_int,
			"action",		dt_byte,
			"custom1",		dt_int,
			"custom2",		dt_int,
			"custom3",		dt_int
	);
	mc_add_packet_type(0xC8, pv_neutral, 4, "statistic-id", dt_int, "amount", dt_byte);
	mc_add_packet_type(0xC9, pv_neutral, 6, "player-name", dt_string, "online", dt_bool, "ping", dt_short);
	mc_add_packet_type(0xCA, pv_neutral, 8,
			"invulnerability",dt_bool,
			"is-flying",	dt_bool,
			"can-fly",		dt_bool,
			"instant-destroy",dt_bool
	);
	mc_add_packet_type(0xFA, pv_neutral, 6,
			"channel",		dt_string,
			"length",		dt_short,
			"data",			dt_special_FA);

	/*mc_add_packet_type(0xFE ...
	*/

	mc_add_packet_type(0xFF, pv_neutral, 2, "reason", dt_string);
}
