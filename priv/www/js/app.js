(function($) {
	var d = new Date();
	var sockjs = null;

	$.app = {}
	$.app.init = function() {
		ui_init();
		sockjs = new SockJS('/a');

		sockjs.onopen = connect;
		sockjs.onclose = disconnect;
		sockjs.onmessage = message;
	}

	/*
		External interface
	*/
	$.app.login = function() {
		var n = $("#nick").val();
		if(n && n.length > 0) {
			ui_login(n);
			send("L:" + n);
		} else ui_login_error("Please, enter correct nickname.");
	}
	$.app.pub = function() { ui_pub(); send("P"); }
	$.app.stop = function() { ui_stop(); send("S"); }
	$.app.send = function() { 
		var v = $("#chat_msg").val();
		if(v && v.length > 0) { $("#chat_msg").val(""); send("C:" + v); };
		return false; }

	/*
		SockJS ops
	*/
	function connect() {
		ui_connect();
		send("I");
	}
	function reconnect() {
		ui_reconnect();
	}
	function disconnect(e) {		
		ui_disconnect();
		setTimeout(reconnect, 3000);
	}
	function send(val) {
		if(sockjs && sockjs.readyState == SockJS.OPEN) 
			sockjs.send(val);
		else
			setTimeout(function() { send(val); }, 1000);
	}
	function message(m) {
		var p = $.parseJSON(m.data); 
		if(p && p.a) {
			switch(p.a) {
				case("lgok"): { p.nick = strip(p.nick); ui_login_ok(p); break; }
				case("lger"): { ui_login_revert(); ui_login_error("Please, choose another one."); break; }
				case("addr"): { p.nick = strip(p.nick); ui_addr(p); break; }
				case("delr"): { ui_hide(p); ui_delr(p); break; }
				case("chat"): { p.nick = strip(p.nick); p.msg = strip(p.msg); break; }
				case("show"): { p.nick = strip(p.nick); ui_show(p); break; }
				case("hide"): { ui_hide(p); break; }	
			}
		}
	}

	/*	
		UI
	*/
	function ui_init() { $(".ctl").hide(); cs("connecting"); $('#nick').tooltip('show'); 
		n = $.cookie('_nick'); if(n) $("#nick").val(n); }
	function ui_login_error(m) { $('#nick').attr('data-original-title', m).tooltip('show');	}
	function ui_login(n) { $("#nick").attr('disabled', 'disabled'); $("#btn_login").hide(); 
		$("#l_login").fadeIn('fast'); $.cookie('_nick', n); }
	function ui_login_revert() { $("#nick").removeAttr('disabled'); $("#btn_login").show('fast'); 	
		$("#l_login").hide(); $(".ctl").hide();  }
	function ui_login_ok(p) { $("#nick_panel").html(p.nick); 
		$("#login_panel").hide(); $(".ctl").slideDown('fast', function() { sync(); embeed_br(p); }); }
	function ui_pub() { $("#btn_pub").fadeOut('fast', function() { $("#btn_stop").fadeIn('fast'); }); }
	function ui_stop() { $("#btn_stop").fadeOut('fast', function() { $("#btn_pub").fadeIn('fast'); }); }

	function ui_connect() { cs("connected"); }
	function ui_disconnect() { cs("disconnect"); $("#broadcast").html(""); $(".ctl").hide(); }
	function ui_reconnect() { cs("connecting"); }

	function ui_addr(m) { $("#rl").prependTo($.nano($("#_roaster").html(), m)); }
	function ui_delr(m) { $("#r_" + m.id).detach(); $("#v_" + m.id); } 
	function ui_chat(m) { $("#log").prependTo($.nano($("#_chat").html(), m)); }
	function ui_show(m) { $("#views").appendTo($.nano($("#_video").html(), m), function() { embeed_v(m); }); }
	function ui_hide(m) { $("#v_" + m.id).html(""); $("#v_" + m.id).detach(); }
		

	/*
		Utils
	*/
	function cs(i) { $(".cs").hide(); $("#" + i).slideDown("fast"); };
	function sync() {
		var rvh = $(".room_video").height();  
		var h = rvh - $(".chat_panel").height() - 20;
		$('.chat_log').height(h > 200 ? h : 200);
		var rch = $(".room_chat").height();
		if(rvh < rch) $(".room_video").height(rch);
	}
	function embeed_br(p) {
		swfobject.embedSWF(
			"/swf/broadcast.swf?" + d.getTime(), 
			"broadcast", 
			$("#broadcast").width(), 
			$("#broadcast").height(),
			"11.0.0", 
			"/swf/expressInstall.swf",
			p);
	}
	function embeed_v(p) {
		var oid = "v_" + p.id; 
		p["width"] = $("#" + oid).width();
		p["height"] = $("#" + oid).height();
		swfobject.embedSWF(
			"/swf/view.swf?" + d.getTime(), 
			oid, 
			p.width, 
			p.height,
			"11.0.0", 
			"/swf/expressInstall.swf",
			p);
	}
	function strip(s) { 
    	s = s.replace(/&/gi, "&amp;");
    	s = s.replace(/\"/gi, "&quot;");
    	s = s.replace(/</gi, "&lt;");
    	s = s.replace(/>/gi, "&gt;");
		return s; }

	/*
		Templates
	*/
	$.nano = function(template, data) {
    	return template.replace(/\{([\w\.]*)\}/g, function (str, key) {
      			var keys = key.split("."), value = data[keys.shift()];
     			$.each(keys, function () { value = value[this]; });
      			return (value === null || value === undefined) ? "" : value;
    		});
  	};

})(window.jQuery);
$(document).ready(function() { $.app.init(); });