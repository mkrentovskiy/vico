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
			ui_login();
			send("L:" + val);
		} else ui_login_error("Please, enter correct nickname.");
	}
	$.app.pub = function() {}
	$.app.stop = function() {}
	$.app.send = function() { return false; }

	/*
		SockJS ops
	*/
	function connect() {
		ui_connect();
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
	}
	function message(m) {

	}

	/*	
		UI
	*/
	function ui_init() { $(".ctl").hide(); cs("connecting"); $('#nick').tooltip('show'); }
	function ui_login_error(m) { $('#nick').attr('data-original-title', m).tooltip('show');	}
	function ui_login() { $("#nick").attr('disabled', 'disabled'); $("#btn_login").hide(); 
		$("#l_login").fadeIn('fast'); }
	function ui_login_revert() { $("#nick").removeAttr('disabled'); $("#btn_login").show('fast'); 	
		$("#l_login").hide(); }
	function ui_login_ok() {}
	
	function ui_connect() { cs("connected"); }
	function ui_disconnect() { cs("disconnect"); $(".ctl").hide(); }
	function ui_reconnect() { cs("connecting"); }

		

	/*
		Utils
	*/
	function cs(i) { $(".cs").hide(); $("#" + i).slideDown("fast"); };
	function strip(s) { return s; }

/*
		swfobject.embedSWF(
			"/swf/broadcast.swf?" + d.getTime(), 
			"broadcast", 
			$("#broadcast").width(), 
			$("#broadcast").height(),
			"11.0.0", 
			"/swf/expressInstall.swf",
			{ "server": "", "stream": "" });
		var h = ($(".room_video").height() - $(".chat_panel").height() - 20);
		$('.chat_log').css('height', (h > 200 ? h : 200) + 'px');

		swfobject.embedSWF(
			"/swf/view.swf?" + d.getTime(), 
			"v1", 
			$("#v1").width(), 
			$("#v1").height(),
			"11.0.0", 
			"/swf/expressInstall.swf",
			{ "server": "", "stream": "", "width": $("#v1").width(), "height": $("#v1").height() });
*/
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