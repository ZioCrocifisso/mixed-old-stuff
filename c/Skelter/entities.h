#ifndef ENTITIES_H_INCLUDED
#define ENTITIES_H_INCLUDED

struct mc_entity_metadata {
	int cfields;
	struct {
		enum {
			dt2_byte,
			dt2_short,
			dt2_int,
			dt2_float,
			dt2_string,
			dt2_slot,
			dt2_3int
		} type;
		char *value;
	} fields[16];
};

struct mc_entity {
	char *name;
	struct mc_entity_metadata metadata;
};

extern void entity_add(int eid);
extern void entity_set_metadata(int eid, struct mc_entity_metadata);
extern void entity_read_metadata(uint8_t **bp, struct mc_entity_metadata *metadata);

#endif // ENTITIES_H_INCLUDED
