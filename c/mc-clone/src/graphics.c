#include <GL/gl.h>
#include "graphics.h"

int mc_graphics_init(mc_gc *context)
{
	if (!context) {
		return 0;
	}

	glClearColor(1.0f, 0.7f, 0.9f, 1.0f);

	return 1;
}

int mc_graphics_clear(mc_gc *context)
{
	if (!context) {
		return 0;
	}

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

int mc_graphics_shaders(mc_gc *context, char *vs_string, char *fs_string)
{
	GLuint vs, fs, program, success, logsize;
	char *log = (char *) 0;

	if (!context) {
		return 0;
	}

	vs = glCreateShader(GL_VERTEX_SHADER);
	glShaderSource(vs, 1, vs_string, strlen(vs_string));
	glCompileShader(vs);
	glGetShaderiv(vs, GL_COMPILE_STATUS, &success);

	if (!success) {
		glGetShaderiv(vs, GL_INFO_LOG_LENGTH, &logsize);
		log = realloc(log, logsize);

		if (log) {
			glGetShaderInfoLog(vs, logsize, &logsize, log);
			mc_error(1, "Vertex shader error: %s", log);
		}

		return 0;
	}

	fs = glCreateShader(GL_FRAGMENT_SHADER);
	glShaderSource(fs, 1, fs_string, strlen(fs_string));
	glCompileShader(fs);
	glGetShaderiv(fs, GL_COMPILE_STATUS, &success);

	if (!success) {
		glGetShaderiv(fs, GL_INFO_LOG_LENGTH, &logsize);
		log = realloc(log, logsize);

		if (log) {
			glGetShaderInfoLog(fs, logsize, &logsize, log);
			mc_error(1, "Fragment shader error: %s", log);
		}

		return 0;
	}

	program = glCreateProgram();
	glAttachShader(program, vs);
	glAttachShader(program, fs);
	glLinkProgram(program);
	glUseProgram(program);

	context->program = program;

	glDetachShader(program, vs);
	glDetachShader(program, fs);
	glDeleteShader(vs);
	glDeleteShader(fs);

	return 1;
}
