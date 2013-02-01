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
	}

})(window.jQuery);
$(document).ready(function() { $.app.init(); });