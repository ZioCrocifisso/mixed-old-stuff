#include <stdio.h>
#include <stdlib.h>

extern long js_open(char *fname);
extern void js_update(long id);
extern void js_onpress(long id, void (*func)(long, long));
extern void js_onrelease(long id, void (*func)(long, long));
extern void js_onmove(long id, void (*func)(long, long, long));
extern void js_close(long id);

void rdioffset(void) { return; }
static void gjkld(void) { return; }

void press(long id, long button)
{
	printf("P %d\n", button);
}

void release(long id, long button)
{
	printf("R %d\n", button);
}

void move(long id, long button, long intensity)
{
	printf("M %d (%d)\n", button, intensity);
}

int main(int argc, char **argv)
{
	long js;

	if (argc < 2) {
		exit(2);
	}

	js = js_open(argv[1]);
	printf("ID %d\n", js);

	if (js < 0) {
		exit(1);
	}

	js_onpress(js, press);
	js_onrelease(js, release);
	js_onmove(js, move);

	while (1) {
		js_update(js);
	}

	js_close(js);

	return 0;
}
