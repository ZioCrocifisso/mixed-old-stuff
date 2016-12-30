#include <GLFW/glfw3.h>
#include <stdio.h>
#include <time.h>

#include "error.h"
#include "graphics.h"
#include "world.h"

int main(int argc, char **argv)
{
	GLFWwindow *window = (GLFWwindow *) 0;
	mc_world world;
	mc_gc context;

	if (!glfwInit()) {
		mc_error(1, "GLFW error.");
	}

	atexit(glfwTerminate);
	glfwSetErrorCallback((GLFWerrorfun) mc_error);

	window = glfwCreateWindow(640, 480, "UntitledCraft", NULL, NULL);

	if (!window) {
		mc_error(1, "Window error.");
	}

	glfwMakeContextCurrent(window);

	if (!mc_graphics_init(&context)) {
		mc_error(1, "Context error.");
	}

	if (!mc_world_init(&world, (unsigned int) time(NULL))) {
		mc_error(1, "World.");
	}

	glfwPollEvents();

	while (!glfwWindowShouldClose(window)) {
		mc_graphics_clear(&context);

		glfwSwapBuffers(window);
		glfwWaitEvents();
	}

	return 0;
}
