/*
 * Copyright (c) 2012-2013, Luca Prezzavento
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met: 
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution. 
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

var game_canvas, context, hmax, vmax, container, mouse_rotation = 0, moves;
var rotated_level = -1, sectheight, sectwidth, prev_mouse_rotation, config;
var gradient1, gradient2, gradient_back, remote = false, req, id;
var is_firefox = navigator.userAgent.indexOf("Firefox") != -1, menu;

function game_initialize() {}
function game_resize() {}
function game_draw_sector(h, v) {}
function game_rotate(v, amount) {}
function game_reverse(h) {}
function game_reverse_single(h) {}
function game_redraw() {}
function game_generate(hmax, vmax) {}
function game_end() {}

function atan2(y, x) {}

function game_remote_start(hm, vm, nick) {}
function game_remote_post(data, block) {}
function game_check_end() {}
function game_remote_end(closed) {}
function game_remote_highs() {}

function game_start() {
	moves = [];

	/*
	if (is_firefox) {
		atans = [];

		for (var x = 0; x < game_canvas.width; x++) {
			atans.push([]);
			for (var y = 0; y < game_canvas.height; y++) {
				atans[x].push(atan2(y, x));
			}
		}
	}
	*/

	window.onresize = game_resize;

	game_canvas.onmousedown = function(e) {
		var x = e.clientX - game_canvas.offsetLeft;
		var y = e.clientY - game_canvas.offsetTop;

		rotated_level = Math.floor(Math.sqrt(
					Math.pow(x - game_canvas.width / 2, 2) +
					Math.pow(y - game_canvas.height / 2, 2)
				) / sectheight);

		prev_mouse_rotation = 0;
		game_canvas.onmousemove(e);
		prev_mouse_rotation = mouse_rotation;
		mouse_rotation = 0;

		if (rotated_level == 0) {
			game_reverse(Math.round((prev_mouse_rotation + Math.PI * 2
				- sectwidth / 2) / (2 * Math.PI) * hmax) % hmax);
			rotated_level = -1;
		}

		game_redraw();
	};

	game_canvas.onmousemove = function(e) {
		var x = e.clientX - game_canvas.offsetLeft;
		var y = e.clientY - game_canvas.offsetTop;

		if (rotated_level >= 0) {
			mouse_rotation = -(atan2(
						(game_canvas.height / 2 - y),
						(x - game_canvas.width / 2)
					)) - prev_mouse_rotation;

			game_redraw();
		}
	};

	game_canvas.onmouseup = function() {
		var rotated_levels = Math.round(mouse_rotation / (2 * Math.PI)
						* hmax);
		if (rotated_level >= 0) {
			game_rotate(rotated_level, rotated_levels);
		}

		rotated_level = -1;
		mouse_rotation = 0;
		game_redraw();

		game_check_end();
	}

	window.onbeforeunload = function() {
		game_remote_end(true);
		return null;
	}

	game_resize();
}

function game_redraw() {
	context.beginPath();
	context.rect(0, 0, game_canvas.width, game_canvas.height);
	context.fillStyle = gradient_back;
	context.fill();

	for (var h = 0; h < hmax; h++) {
		for (var v = 0; v < vmax; v++) {
			game_draw_sector(h, v);
		}
	}
}

function game_draw_sector(h, v) {
	var addrot = (rotated_level == v) ? mouse_rotation : 0;
	var mul = (config[v][h] == 2) + 1;

	if (h > 0) {
		if (config[v][h - 1] == 2) {
			return;
		}
	}

	context.beginPath();

	if (v > 0) {
		for (i = 0; i <= 1; i++) {
			context.arc(
				game_canvas.width / 2,
				game_canvas.height / 2,
				(v + i) * sectheight,
				(sectwidth) * (h + i * mul) + addrot,
				(sectwidth) * (h + !i * mul) + addrot,
				i
			);
			context.lineTo(
				game_canvas.width / 2 +
				sectheight * (v + !i) * Math.cos(sectwidth * (h + !i * mul) + addrot),
				game_canvas.height / 2 +
				sectheight * (v + !i) * Math.sin(sectwidth * (h + !i * mul) + addrot)
			);
		}
	} else {
		context.moveTo(game_canvas.width / 2, game_canvas.height / 2);
		context.lineTo(
			game_canvas.width / 2 +
			sectheight * Math.cos(sectwidth * h),
			game_canvas.height / 2 +
			sectheight * Math.sin(sectwidth * h)
		);

		context.arc(
			game_canvas.width / 2,
			game_canvas.height / 2,
			sectheight,
			sectwidth * h,
			sectwidth * (h + mul)
		);
		context.lineTo(game_canvas.width / 2, game_canvas.height / 2);
	}

	context.closePath();
	context.lineWidth = 2;

	if (mul == 2)
		h++;

	if (config[v][h] == 1)
		context.fillStyle = gradient1;
	else
		context.fillStyle = gradient2;

	context.fill();
	context.stroke();
}

function game_rotate(v, amount) {
	var len = config[v].length;

	if (amount < 0) {
		amount += hmax;
	}

	if (config[v][0] == 2 && amount % 2 != 0)
		return;

	config[v] = config[v].slice(
					len - amount,
					len
				).concat(
					config[v].slice(
						0,
						len - amount
					)
				);
	if (remote) {
		moves.push({
				"type": "rot",
				"v": v,
				"a": amount
		});
	}
}

function game_reverse(h) {
	var ch = h;

	if (config[0][h] == 2) {
		ch = (h + 1) % hmax;
		game_reverse_single(ch);
	} else if (h >= 1) {
		if (config[0][h - 1] == 2) {
			game_reverse_single(h - 1);
		}
	}

	config[0][ch] = !config[0][ch];

	game_reverse_single(h);

	if (remote) {
		moves.push({
				"type": "rev",
				"h": h
		});
	}
}

function game_reverse_single(h) {
	var ch;

	for (var v = 1; v < vmax; v++) {
		if (config[v][h] == 2)
			ch = (h + 1) % hmax;
		else
			ch = h;

		config[v][ch] = !config[v][ch];
	}
}

var _qPI = Math.PI / 4;

function atan2(y, x) {
	/*
	if (x < atans.length) {
		if (y < atans[x].length) {
			return atans[x][y];
		}
	}
	*/

	if (is_firefox) {
		var ay = Math.abs(y);
		var xay = x + ay;
		var angle;

		if (x >= 0) {
			angle = _qPI - _qPI * ((x - ay) / xay);
		} else {
			angle = _qPI * 3 - (xay / (ay - x));
		}

		if (y < 0) {
			return -angle;
		} else {
			return angle;
		}
	} else {
		return Math.atan2(y, x);
	}
}

function game_generate(hm, vm) {
	hmax = hm;
	vmax = vm;
	config = [];

	for (v = 0; v < vm; v++) {
		var row = [];

		for (h = 0; h < hm; h++) {
			if (v == 0 && h % 2 == 0 && hmax % 2 == 0) {
				row.push(2);
			} else {
				row.push(1);
			}
		}

		config.push(row);
	}

	for (var i = 0; i < 200; i++) {
		if (Math.random() >= 0.4) {
			game_rotate(Math.floor(Math.random() * (vmax - 1)) + 1,
					Math.floor(Math.random() * (hmax - 1)));
		} else {
			game_reverse(Math.floor(Math.random() * hmax));
		}
	}
}

function game_remote_post(data, block) {
	var str = "", xhr;

	for (key in data) {
		str += key + "=" + data[key] + "&";
	}

	str = str.substr(0, str.length - 1);

	xhr = new XMLHttpRequest();
	xhr.open("POST", "csal.php", !block);
	xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
	xhr.send(str);

	return xhr;
}

function game_remote_start(hm, vm, nick) {
	req = game_remote_post({
					"action": "start",
					"hmax": hm,
					"vmax": vm,
					"nick": nick
				});

	hmax = hm;
	vmax = vm;

	req.onreadystatechange = function() {
		var jsonresp;

		if (req.readyState == 4) {
			var jsonvalid;
			try {
				jsonresp = JSON.parse(req.responseText);
				jsonvalid = true;
			} catch (e) {
				jsonvalid = false;
			}

			if (req.status == 200 && jsonvalid) {
				config = jsonresp.config;
				id = jsonresp.id;
				remote = true;
				game_start();
			} else {
				alert("Impossibile creare una partita remota: \n" +
					req.responseText + "\nYour score won't be saved.");
				remote = false;
				game_generate(hmax, vmax);
				game_start();
			}
		}
	}
}

function game_check_end() {
	for (var h = 0; h < hmax; h++) {
		for (v = 0; v < vmax; v++) {
			if (config[v][h] == 0) {
				return;
			}
		}
	}

	if (remote)
		game_remote_end();
	else
		game_end();
}

function game_remote_end(closed) {
	req = game_remote_post({
					"action": "end",
					"id": id,
					"moves": JSON.stringify(moves)
				}, closed);

	if (!closed) {
		req.onreadystatechange = function() {
			if (req.readyState == 4) {
				if (req.status == 200) {
					alert(req.responseText);
				}
				game_end();
			}
		}
	}
}

function game_end() {
	remote = false;
	
	game_canvas.onmouseup = null;
	game_canvas.onmousedown = null;
	game_canvas.onmousemove = null;
}

function game_initialize() {
	menu = document.getElementById("menu");
	game_canvas = document.getElementById("game");
	context = game_canvas.getContext("2d");
	container = document.getElementById("container");

	if (localStorage.getItem('nickname') != null) {
		document.getElementById("inick").value = localStorage.getItem('nickname');
	}

	document.getElementById("iremote").onclick = function() {
		if (document.getElementById("iremote").checked) {
			document.getElementById("inick").disabled = "";
		} else {
			document.getElementById("inick").disabled = "disabled";
		}
	}

	document.getElementById("isubmit").onclick = function() {
		var hm, vm;

		hm = parseInt(document.getElementById("ihmax").value);
		vm = parseInt(document.getElementById("ivmax").value);

		if (remote) {
			game_remote_end(true);
		}

	
		localStorage.setItem('nickname', document.getElementById("inick").value);

		if (document.getElementById("iremote").checked) {
			game_remote_start(hm, vm, document.getElementById("inick").value);
		} else {
			remote = false;
			game_generate(hm, vm);
			game_start();
		}
	}

	document.getElementById("ihighs").onclick = function() {
		var highs = document.getElementById("highs");
	
		if (highs.style.display == "none") {
			highs.style.display = "block";
		} else {
			highs.style.display = "none";
		}

		game_remote_highs();
		game_resize();
	}
}

function game_remote_highs() {
	document.getElementById("highs").innerText = "Caricamento...";
	req = game_remote_post({ "action": "high" });

	req.onreadystatechange = function () {
		if (req.readyState == 4) {
			var text = "";

			if (req.status == 200) {
				var highs = JSON.parse(req.responseText);

				highs.sort(function(a, b) {
					return b.score - a.score;
				});

				for (i in highs) {
					text += "<p><b>" + (parseInt(i) + 1) + ".</b> " +
						highs[i].nick + " " + highs[i].score + "</p>";
				}
			} else {
				text = "Caricamento fallito: " + req.responseText;
			}

			document.getElementById("highs").innerHTML = text;
		}
	}
}

function game_resize() {
	game_canvas.width = container.getClientRects()[0].width;
	game_canvas.height = container.getClientRects()[0].height
				- menu.getClientRects()[0].height;
	sectheight = (Math.min(game_canvas.height, game_canvas.width) / (vmax + 1)) / 2;
	sectwidth = Math.PI * 2 / hmax;

	gradient1 = context.createRadialGradient(
						game_canvas.width / 2,
						game_canvas.height / 2,
						0,
						game_canvas.width / 2,
						game_canvas.height / 2,
						sectheight * vmax
					);
	gradient1.addColorStop(0, "#55555F");
	gradient1.addColorStop(1, "#222222");

	gradient2 = context.createRadialGradient(
						game_canvas.width / 2,
						game_canvas.height / 2,
						0,
						game_canvas.width / 2,
						game_canvas.height / 2,
						sectheight * vmax
					);
	gradient2.addColorStop(0, "#FFFFFF");
	gradient2.addColorStop(1, "#AAAAAA");

	gradient_back = context.createLinearGradient(
						0, 0, 0,
						game_canvas.height);
	gradient_back.addColorStop(0, "#818181");
	gradient_back.addColorStop(1, "#3F3F3F");

	game_redraw();
}

/*
     FILE ARCHIVED ON 02:09:27 Oct 02, 2014 AND RETRIEVED FROM THE
     INTERNET ARCHIVE ON 01:01:27 Mar 04, 2018.
     JAVASCRIPT APPENDED BY WAYBACK MACHINE, COPYRIGHT INTERNET ARCHIVE.

     ALL OTHER CONTENT MAY ALSO BE PROTECTED BY COPYRIGHT (17 U.S.C.
     SECTION 108(a)(3)).
*/
