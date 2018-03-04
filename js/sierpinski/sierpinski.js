function timer(state) {
        var params;

        params = state.paramfunc(null);

        if (params.rotate) {
                state.rotation += 0.01;
        }

        if (state.rotation >= Math.PI) {
                state.rotation = -Math.PI;
        }

        if (params.iterchanged) {
                state.buffer = generate(state.gl, state.program, params.iter);
                params.iterchanged = false;
        }

        state.paramfunc(params);

        state.gl.uniform1f(
                                state.gl.getUniformLocation(state.program, "rot"),
                                state.rotation
        );

        draw(state.gl, state.buffer, state.program);
}

function draw(gl, buffer, program) {
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
        gl.drawArrays(gl.TRIANGLES, 0, buffer.count);
}

function pyramid(vertices, x, y, z, sz, min) {
        var ms = sz / 2;

        if (sz > min) {
                vertices = pyramid(vertices, x, y + ms, z, ms, min);
                vertices = pyramid(vertices, x - ms, y - ms, z - ms, ms, min);
                vertices = pyramid(vertices, x + ms, y - ms, z - ms, ms, min);
                vertices = pyramid(vertices, x - ms, y - ms, z + ms, ms, min);
                vertices = pyramid(vertices, x + ms, y - ms, z + ms, ms, min);
        } else {
                var ph, p = [];

                ph = [x, y + sz, z];
                p[0] = [x - sz, y - sz, z - sz];
                p[1] = [x - sz, y - sz, z + sz];
                p[3] = [x + sz, y - sz, z - sz];
                p[2] = [x + sz, y - sz, z + sz];

                for (var i = 0; i < 4; i++) {
                        vertices = triangle(vertices, ph, p[i], p[(i + 1) % 4]);
                }

                vertices = triangle(vertices, p[0], p[1], p[2]);
                vertices = triangle(vertices, p[1], p[2], p[3]);
        }

        return vertices;
}

function triangle(vertices, p1, p2, p3) {
        return vertices.concat(p1.concat(p2).concat(p3));
}

function generate(gl, program, iter) {
        var buffer = gl.createBuffer(), vertices = [], attr, params;

        gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
        vertices = pyramid(vertices, 0.0, 0.2, -2.3, 1.0, 1.0 / iter);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW);
        attr = gl.getAttribLocation(program, "pos");
        gl.enableVertexAttribArray(attr);
        gl.vertexAttribPointer(attr, 3, gl.FLOAT, false, 0, 0);
        buffer.count = vertices.length / 3;

        return buffer;
}

function loadShader(gl, filename, type) {
        /*
        var xhr = new XMLHttpRequest;

        xhr.open("GET", filename, false);
        xhr.send(null);

        if (xhr.status == 200) {
                var shader = gl.createShader(type);

                gl.shaderSource(shader, xhr.responseText);
                gl.compileShader(shader);

                return shader;
        }

        return null;
        */
        var src;

        //Essendo una versione adatta all'esecuzione locale, gli shader
        //sono stati trasformati automaticamente in stringhe Javascript
        //e inclusi nello script.
        if (filename == "vs") {
                src =
                "attribute vec3 pos;" +
                "" +
                "varying vec4 vcol;" +
                "" +
                "uniform float rot;" +
                "" +
                "float col(float pos) {" +
                "       float r = abs(mod(pos, 0.05) * 20.0);" +
                "" +
                "       if (r < 0.2) {" +
                "               r = 1.0 - r;" +
                "       }" +
                "" +
                "       return r;" +
                "}" +
                "" +
                "void main(void)" +
                "{" +
                "       gl_Position = vec4(pos.x, pos.y, pos.z + 2.0, 2.0);" +
                "" +
                "       vcol = vec4(col((pos.y + pos.z) / 2.0), col((pos.x + pos.y) / 2.0), col((pos.x + pos.z) / 2.0), 1.0);" +
                "       mat4 rotm = mat4(" +
                "               cos(rot),       0.0,            -sin(rot),      0.0," +
                "               0.0,            1.0,            0.0,            0.0," +
                "               sin(rot),       0.0,            cos(rot),       0.0," +
                "               0.0,            0.0,            0.0,            1.0" +
                "       );" +
                "" +
                "       gl_Position *= rotm;" +
                "       gl_Position.z -= 2.0;" +
                "" +
                "       mat4 prjm = mat4(" +
                "               0.9,    0.0,    0.0,    0.0," +
                "               0.0,    0.9,    0.0,    0.0," +
                "               0.0,    0.0,    -1.0,   -0.01," +
                "               0.0,    0.0,    -1.0,   0.0" +
                "       );" +
                "" +
                "" +
                "       gl_Position *= prjm;" +
                "}";
        } else {
                src =
                "precision mediump float;" +
                "varying vec4 vcol;" +
                "" +
                "void main(void)" +
                "{" +
                "       gl_FragColor = vcol;" +
                "}";
        }

        var shader = gl.createShader(type);

        gl.shaderSource(shader, src);
        gl.compileShader(shader);

        return shader;
}

function init(paramfunc) {
        var gl, canvas, program, buffer, vs, fs, state = {};

        canvas = document.getElementById("canvas");
        gl = canvas.getContext("webgl");

        if (!gl) {
                gl = canvas.getContext("experimental-webgl");
        }

        program = gl.createProgram();
        vs = loadShader(gl, "vs", gl.VERTEX_SHADER);
        gl.attachShader(program, vs);
        fs = loadShader(gl, "fs", gl.FRAGMENT_SHADER);
        gl.attachShader(program, fs);
        gl.bindAttribLocation(program, 0, "pos");
        gl.linkProgram(program);
        gl.useProgram(program);

        gl.clearColor(0.0, 0.0, 0.0, 1.0);
        gl.viewport(0, 0, canvas.width, canvas.height);
        gl.enable(gl.DEPTH_TEST);
        gl.depthFunc(gl.LESS);

        state.gl = gl;
        state.program = program;
        state.buffer = buffer;
        state.rotation = 0;
        state.paramfunc = paramfunc;

        setInterval(timer, 20, state);
}
