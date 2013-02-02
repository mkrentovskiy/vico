(function($) {
	$.app = {}
	$.app.init = function() {
		d = new Date();
		swfobject.embedSWF(
			"/swf/broadcast.swf?" + d.getTime(), 
			"broadcast", 
			$("#broadcast").width(), 
			$("#broadcast").height(),
			"11.0.0", 
			{ server: "", stream: "" });
		var h = ($(".room_video").height() - 60);
		$('.chat_log').css('height', (h > 200 ? h : 200) + 'px');
	}

})(window.jQuery);
$(document).ready(function() { $.app.init(); });