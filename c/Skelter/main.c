#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <math.h>
#include "mcclient.h"

//TODO: special 0x3C

void printmcs(wchar_t *str);

int main()
{
	mc client;
	mce *e;
	mc_block block;
	wchar_t *msg, *umsg = L"", smsg[16] = L"";
	int i, health = 0;
	double angle = 0.0, xx = 0.0, yy = 0.0, zz = 0.0, speed = 1;
	char circ = 0;

	printf("%s\r\n", strerror(mc_connect(&client, "localhost", 25565, L"Skelter")));	
	//printf("%s\r\n", strerror(mc_connect(&client, "178.33.230.112", 25594, L"Skelter")));
	
	while ((e = mc_next_event(&client))) {
		if (!yy && client.y) {
			xx = client.x;
			yy = client.y;
			zz = client.z;
		}
		
		if (e->type == ev_login) {
			mc_send_message(&client, L"/login p");
		} else if (e->type == ev_chat_message) {
			msg = ((struct mc_event_chat_message *) e)->message;
			printmcs(msg);
			puts("");
			
			for (i = 0; i < wcslen(msg); i++) {
				if (65 <= msg[i] && msg[i] <= 90)
					msg[i] += 32;
			}
			
			umsg = wcsstr(msg, L" ");
			
			if (umsg && !wcsstr(umsg, L"moveo")) {
				if (wcsstr(msg, L"<ziocrocifisso> exi")) {
					mc_send_message(&client, L"exeo.");
					break;
				} /*else if (wcsstr(umsg, L"i huc")) {
					mc_goto(&client, 
				}*/else if (wcsstr(umsg, L"move+")) {
					mc_send_message(&client, L"moveo amplius.");
					speed += 1;
				} else if (wcsstr(umsg, L"move-")) {
					mc_send_message(&client, L"moveo minus.");
					speed -= 1;
				} else if (wcsstr(umsg, L"move@")) {					
					if (circ)
						mc_send_message(&client, L"non moveo perpetuo.");
					else
						mc_send_message(&client, L"moveo perpetuo.");
						
					circ = !circ;
					
				} else if (wcsstr(umsg, L"move")) {
					mc_send_message(&client, L"moveo.");
					mc_look(&client, client.yaw + 0.1, client.pitch - 0.1);
					mc_goto(&client, client.x + speed, client.y, client.z + speed);
				} else if (wcsstr(umsg, L"db")) {
					block = mc_get_block(&client, (int) floor(client.x), ((int) floor(client.y)) - 1, (int) floor(client.z));
					smsg[0] = L'\0';
					swprintf(smsg, 16, L"%d, %d", block.id, block.block_light); 
					mc_send_message(&client, smsg);
				}
			}
			
		} else if (e->type == ev_disconnect) {
			break;
		} else if (e->type == ev_update && yy && circ) {
			if (angle >= 360)
				angle = 0.5;
			else
				angle += 0.5;
			
			mc_goto(&client, floor(xx) + cos(angle * 0.017453292) * speed * 3, yy, floor(zz) + sin(angle * 0.017453292) * speed * 3);
		} else if (e->type == ev_death) {
			mc_respawn(&client);
			mc_send_message(&client, L"!!");
		} else if (e->type == ev_health) {
			if (((struct mc_event_health *) e)->health < health) {
				mc_send_message(&client, L"!");
			}
			
			health = ((struct mc_event_health *) e)->health;
		}

		free(e);
		e = NULL;
	}

	puts("Disconnected");
	mc_disconnect(&client);
	
	if (e)
		free(e);
		
	getchar();

	return 0;
}

void printmcs(wchar_t *str)
{
	for (; *str; str++) {
		if (*str == L'§') {
			str++;
		} else {
			putwchar(*str);
		}
	}
}
