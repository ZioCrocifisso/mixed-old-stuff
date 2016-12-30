#include <string.h>
#include <wchar.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>

#include "connection.h"
#include "entities.h"
#include "player.h"

static int recvl(int fd, char *buffer, int size, int flags);
static void read_string_prefix(uint8_t **bp, int16_t *len);
static void read_string(uint8_t **bp, int16_t len, wchar_t *out);
static void free_fields(struct mc_packet *p);
static struct mc_packet packet_types[256][2];

int mc_connect(mc *client, char *hostname, unsigned short port, wchar_t *username)
{
	struct sockaddr_in addr;
	struct hostent *he;
	struct mc_packet handshake;
	wchar_t hstring[1025], whostname[1024];
	int i;

	mc_add_packet_types();

	client->username = username;
	client->socket = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
	client->cevents = 0;
	client->login = 0;

	client->player_id = -1;
	client->gamemode = creative;
	client->dimension = overworld;
	client->difficulty = normal;
	
	client->x = 0;
	client->y = 0;
	client->z = 0;
	client->on_ground = 1;
	client->stance = MC_PLAYER_HEIGHT;
	client->yaw = 0;
	client->pitch = 0;
	
	client->spawn = 0;
	client->moving = 1;
	client->looking = 1;
	client->flying = 0;
	
	client->columns = NULL;

	if (!client->socket)
		return errno;

	addr.sin_port = htons(port);
	addr.sin_family = AF_INET;

	he = gethostbyname(hostname);

	if (!he)
		return errno;

	addr.sin_addr = * ((struct in_addr *) he->h_addr_list[0]);

	if (connect(client->socket, (struct sockaddr *) &addr, sizeof(struct sockaddr)) == -1)
		return errno;

	mbstowcs(whostname, hostname, strlen(hostname));

	hstring[0] = 0;
	wcscat(hstring, username);
	wcscat(hstring, L";");
	wcscat(hstring, whostname);

	mc_packet_new(&handshake, 0x02);
	mc_packet_set(&handshake, "user-host", hstring, wcslen(hstring) * sizeof(wchar_t));
	mc_packet_send(client, handshake);

	return 0;
}

int mc_packet_new(struct mc_packet *packet, uint8_t id)
{
	return memcpy(packet, &packet_types[id][pv_fromclient], sizeof(struct mc_packet)) != NULL;
}

static int mc_packet_news(struct mc_packet *packet, uint8_t id)
{
	return memcpy(packet, &packet_types[id][pv_fromserver], sizeof(struct mc_packet)) != NULL;
}

int mc_packet_set(struct mc_packet *packet, char *field, void *data, int size)
{
	int i;

	for (i = 0; i < packet->cfields; i++) {
		if (strcmp(packet->fields[i].name, field) == 0) {
			packet->fields[i].value = data;
			packet->fields[i].size = size;
		}
	}
}

int mc_packet_get(struct mc_packet *packet, char *field, void **data, void *copy)
{
	int i;

	for (i = 0; i < packet->cfields; i++) {
		if (strcmp(packet->fields[i].name, field) == 0) {
			if (copy)
				memcpy(copy, packet->fields[i].value, packet->fields[i].size);

			if (data)
				*data = packet->fields[i].value;

			return packet->fields[i].size;
		}
	}

	return 0;
}

int mc_packet_send(mc *client, struct mc_packet packet)
{
	#define wn(type)													\
		for (j = 0; j < sizeof(type); j++) {							\
			*(bp++) = valuebytes[sizeof(type) - j - 1];				 	\
		}

	/*#define wnr(type)													\
		for (j = 0; j < sizeof(type); j++) {							\
			*(bp++) = valuebytes[j];									\
		}
	*/

	uint8_t buffer[1024], *bp = buffer, *bp2, *valuebytes;
	uint64_t zero = 0;
	wchar_t *wzero = L"";
	int i, j, l, size = 256;

	if (packet.id > 0 && packet.id != 0x0A)
		printf("Sent: \t0x%02X\n", packet.id);

	*(bp++) = packet.id;

	for (i = 0; i < packet.cfields; i++) {
		if (strlen(packet.fields[i].name) == 0) {
			if (packet.fields[i].dt == dt_string)
				valuebytes = (char *) &wzero;
			else
				valuebytes = (char *) &zero;
		} else {
			valuebytes = ((char *) packet.fields[i].value);
		}

		switch (packet.fields[i].dt) {
			case dt_bool:
			case dt_byte:
				wn(int8_t);
				break;

			case dt_ubyte:
				wn(uint8_t);
				break;

			case dt_short:
				wn(int16_t);
				break;

			case dt_ushort:
				wn(uint16_t);
				break;

			case dt_int:
				wn(int32_t);
				break;

			case dt_long:
				wn(int64_t);
				break;

			case dt_float:
				wn(float);
				break;

			case dt_double:
				wn(double);
				break;

			case dt_string:
				l = packet.fields[i].size / sizeof(wchar_t);
				*(bp++) = ((char *) &l)[1];
				*(bp++) = ((char *) &l)[0];

				for (j = 0; j < l; j += 1) {
					*(bp++) = valuebytes[j * sizeof(wchar_t) + 1];
					*(bp++) = valuebytes[j * sizeof(wchar_t)];
				}
				break;

			default:
				puts("unimplemented DT sent");
		}
	}

	send(client->socket, buffer, bp - buffer, 0);

	/*printf("Sending: ");

	for (bp2 = buffer; bp2 < bp; bp2++)
		printf("%02X ", *bp2);


	if (errno) {
		printf("[ERROR %d: %s]\n", errno, strerror(errno));
	} else
		puts("");
	*/

	return errno;
}

int mc_packet_recv(mc *client)
{
	#define rn(type)												\
		new.fields[i].size = sizeof(type);							\
		new.fields[i].value = malloc(sizeof(type));					\
		for (bp2 = new.fields[i].value + sizeof(type) - 1;			\
				(void *) bp2 >= new.fields[i].value; bp2--) {		\
					*bp2 = *(bp++);									\
				}

	/*#define rnr(type)												\
		new.fields[i].size = sizeof(type);							\
		new.fields[i].value = malloc(sizeof(type));					\
		for (bp2 = new.fields[i].value;								\
				(void *) bp2 < new.fields[i].value + sizeof(type);	\
				bp2++) {											\
					*bp2 = *(bp++);									\
				}
	*/

	struct mc_packet new;
	static uint8_t buffer[20480];
	static int size = 0;
	uint8_t *bp, *bp2, tid;
	int tsize, i, j, k = 0;

	tsize = recvl(client->socket, buffer + size, 20480 - size, 0);

	/*if (size) {
		printf("Recvd: ");
		for (i = 0; i < size; i++) {
			printf("%02X ", (unsigned char) buffer[i]);
		}
		printf("\n");
	}*/

	if (tsize == 0) {
		client->connection_status = disconnected;
		return 0;
	} else if (tsize > 0) {
		size += tsize;
	}

	while (size) {
		bp = buffer;
		new.cfields = 0;
		
		tid = *(bp++);
		mc_packet_news(&new, (uint8_t) tid);
		
		if (new.id != tid) {
			printf("!!! Unknown packet: 0x%02X !!!", tid);
		}
		
		if (new.id != 0x23 && new.id != 0x20 && new.id != 0x1C && new.id != 0x1A && new.id != 0x1F
			&& new.id != 0x21 && new.id != 0x1D && new.id != 0x00 && new.id != 0x04 && new.id != 0x03
			&& new.id != 0x18 && new.id != 0xFF && new.id != 0x22 && new.id != 0x28
			&& new.id != 0x29 && new.id != 0x05 && new.id != 0x32 && new.id != 0x33 && 1) {
			printf("Recvd: \t0x%02X [tsize: %d, size: %d]\n", new.id, tsize, size);
		}

		for (i = 0; i < new.cfields; i++) {
			if (bp >= buffer + size) {
				free_fields(&new);
				goto exitwhile;
			}
			
			switch (new.fields[i].dt) {
				case dt_bool:
				case dt_byte:
					rn(int8_t);
					break;

				case dt_ubyte:
					rn(uint8_t);
					break;

				case dt_short:
					rn(int16_t);
					break;

				case dt_ushort:
					rn(uint16_t);
					break;

				case dt_int:
					rn(int32_t);
					break;

				case dt_long:
					rn(int64_t);
					break;

				case dt_float:
					rn(float);
					break;

				case dt_double:
					rn(double);
					break;

				case dt_string:
					read_string_prefix(&bp, (int16_t *) &new.fields[i].size);
					new.fields[i].value = malloc(new.fields[i].size);
					read_string(&bp, (int16_t) new.fields[i].size, (wchar_t *) new.fields[i].value);
					break;

				case dt_metadata:
					new.fields[i].size = sizeof(struct mc_entity_metadata);
					new.fields[i].value = malloc(sizeof(struct mc_entity_metadata));

					entity_read_metadata(&bp, (struct mc_entity_metadata *) new.fields[i].value);
					break;

				case dt_chunk:
					mc_packet_get(&new, "compressed-size", NULL, &new.fields[i].size);
					new.fields[i].value = malloc(new.fields[i].size);
					
					if (bp + new.fields[i].size >= buffer + size) {
							free_fields(&new);
							goto exitwhile;
					}
					for (j = 0; j < new.fields[i].size; j++) {
						((uint8_t *) new.fields[i].value)[j] = *(bp++);
					}
					
					break;

				case dt_slot:
					new.fields[i].value = malloc(sizeof(struct mc_item_slot));
					new.fields[i].size = mc_read_slot(&bp, new.fields[i].value);
					break;

				case dt_special_68:
					mc_packet_get(&new, "count", NULL, &k);

					new.fields[i].value = malloc(sizeof(struct mc_item_slot) * k);
					new.fields[i].size = 0;

					for (j = 0; j < k; j++) {
						new.fields[i].size += mc_read_slot(&bp, (struct mc_item_slot *) (new.fields[i].value + (j * sizeof(struct mc_item_slot))));
					}
					break;
					
				case dt_special_34:
					mc_packet_get(&new, "data-size", NULL, &new.fields[i].size);
					new.fields[i].value = malloc(new.fields[i].size);
					
					for (j = 0; j < new.fields[i].size; j++) {
						((uint8_t *) new.fields[i].value)[j] = *(bp++);
					}
					break;
					
				case dt_special_FA:
					mc_packet_get(&new, "length", NULL, &new.fields[i].size);
					new.fields[i].value = malloc(new.fields[i].size);
					
					for (j = 0; j < new.fields[i].size; j++) {
						((uint8_t *) new.fields[i].value)[j] = *(bp++);
					}
					break;
			}
		}


		if (new.id == 0x82) {
			new.id = 0x82;
		}

		if (mc_packet_handlers[new.id])
			mc_packet_handlers[new.id](client, new);

		free_fields(&new);
		
		size -= bp - buffer;
		memmove(buffer, bp, size);
	}
	
	exitwhile:

	return tsize;
}

int mc_disconnect(mc *client)
{
	close(client->socket);

	return errno;
}

int mc_add_packet_type(int id, enum mc_packet_version nversion, int cfields, ...)
{
	int i;
	enum mc_packet_version version = pv_neutral;
	va_list va;

	if (nversion == pv_neutral)
		version = pv_fromserver;
	else
		version = nversion;

	va_start(va, cfields);

	packet_types[id][version].id = id;
	packet_types[id][version].version = version;
	packet_types[id][version].cfields = cfields / 2;

	for (i = 0; i < cfields; i++) {
		packet_types[id][version].fields[i].name = va_arg(va, char *);
		packet_types[id][version].fields[i].dt = va_arg(va, enum mc_data_type);
	}

	if (nversion == pv_neutral)
		memcpy(&packet_types[id][pv_fromclient], &packet_types[id][pv_fromserver], sizeof (struct mc_packet));

	va_end(va);

	return 1;
}

void free_fields(struct mc_packet *p)
{
	int i, j;
	struct mc_entity_metadata metadata;

	for (i = 0; i < p->cfields; i++) {
		if (p->fields[i].dt == dt_metadata && p->fields[i].value) {
			metadata = * (struct mc_entity_metadata *) p->fields[i].value;
			for (j = 0; j < metadata.cfields; j++) {
				if (metadata.fields[j].value);
					free(metadata.fields[j].value);
			}
		}
		
		if (p->fields[i].value)
			free(p->fields[i].value);
	}

	p->cfields = 0;
}

#define next *((*bp)++)

extern void entity_read_metadata(uint8_t **bp, struct mc_entity_metadata *metadata)
{
	#define rn2(type)																	\
		metadata->fields[metadata->cfields].value = malloc(sizeof(type));				\
		for (i = sizeof(type) - 1; i >= 0; i--) {										\
			((char *) metadata->fields[metadata->cfields].value)[i] = next;				\
		}

	#define rno(type, offset)															\
		for (i = sizeof(type) - 1; i >= 0; i--) {										\
			((char *) metadata->fields[metadata->cfields].value)[i + offset] = next;	\
		}

	unsigned char mdb, mdtype, mdindex;
	int i;
	int16_t l;

	metadata->cfields = 0;

	do {
		mdb = next;

		if (mdb == 127)
			break;

		mdindex = mdb & 0x1f;
		mdtype = mdb >> 5;

		switch (mdtype) {
			case dt2_byte:
				rn2(char);
				break;

			case dt2_short:
				rn2(int16_t);
				break;

			case dt2_int:
				rn2(int32_t);
				break;

			case dt2_float:
				rn2(float);
				break;

			case dt2_string:
				read_string_prefix(bp, &l);
				metadata->fields[metadata->cfields].value = malloc((l + 1) * sizeof(wchar_t));
				read_string(bp, l, (wchar_t *) metadata->fields[metadata->cfields].value);
				((wchar_t *) metadata->fields[metadata->cfields].value)[l] = 0;
				break;

			case dt2_slot:
				metadata->fields[metadata->cfields].value = malloc(5);
				rno(int16_t, 0);
				rno(int8_t, 2);
				rno(int16_t, 3);
				break;

			case dt2_3int:
				metadata->fields[metadata->cfields].value = malloc(12);
				rno(int32_t, 0);
				rno(int32_t, 4);
				rno(int32_t, 8);
				break;
		}

		metadata->cfields++;
	} while (1);

}

static void read_string_prefix(uint8_t **bp, int16_t *len)
{
	*len = 0;
	((uint8_t *) len)[1] = next;
	((uint8_t *) len)[0] = next;
	*len *= sizeof(wchar_t);
}

static void read_string(uint8_t **bp, int16_t len, wchar_t *out)
{
	int j;

	len /= 4;
	for (j = 0; j < len; j += 1) {
		((uint8_t *) out)[j * 4 + 3] = 0;
		((uint8_t *) out)[j * 4 + 2] = 0;
		((uint8_t *) out)[j * 4 + 1] = next;
		((uint8_t *) out)[j * 4] = next;
	}
}

int mc_read_slot(uint8_t **bp, struct mc_item_slot *slot)
{
	int i;
	int16_t size;
	uint8_t *ibp = *bp;

	((char *) &slot->id)[1] = next;
	((char *) &slot->id)[0] = next;

	if (slot->id > -1) {
		slot->count = next;

		((char *) &slot->damage)[1] = next;
		((char *) &slot->damage)[0] = next;

		if ((256 <= slot->id && slot->id <= 259) ||
			(267 <= slot->id && slot->id <= 279) ||
			(283 <= slot->id && slot->id <= 286) ||
			(290 <= slot->id && slot->id <= 294) ||
			(298 <= slot->id && slot->id <= 317) ||
			slot->id == 261 || slot->id == 359 ||
			slot->id == 346) {
				((char *) &size)[1] = next;
				((char *) &size)[0] = next;

				for (i = 0; i < size; i++) {
					next;
				}
			}
	}

	return (int) (*bp - ibp);
}

int recvl(int fd, char *buffer, int size, int flags)
{
	fd_set set;
	struct timeval tv;
	
	tv.tv_sec = 0;
	tv.tv_usec = 50000;
	
	FD_ZERO(&set);
	FD_SET(fd, &set);
	
	select(fd + 1, &set, NULL, NULL, &tv);
	
	if (FD_ISSET(fd, &set)) {
		return recv(fd, buffer, size, flags);
	} else {
		return -1;
	}
}
