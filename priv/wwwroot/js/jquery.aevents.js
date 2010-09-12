/*
	AEvents for jQuery
	(c) DevImpress, 2010
	http://devimpress.com/
*/ 
	

(function($) {

	var AE_MSG_MAX = 3;
	var AE_TIMEOUT_ADD = 100;	
	var AE_TIMEOUT_MIN = 500;	
	var AE_TIMEOUT_D = 100;
		
 	$.AEvents = function(tcb, errorcb) {
 		$.AEvents.testCallback = tcb; 		 		
 		$.AEvents.errorCallback = errorcb; 		 		
 		$.AEvents.messageLoop();
	}, 
	
	$.AEvents.map = [],
	$.AEvents.testCallback = null,
	$.AEvents.errorCallback = null,
	
	$.AEvents.query = [],
	$.AEvents.mutex = true,
	$.AEvents.mutexSend = true,
	$.AEvents.timeout = 1000, 
	$.AEvents.canSend = false, 
	
	
	/*
	 * 	Transfer events
	 */

	$.AEvents.addMessage = function(m) {
		if($.AEvents.mutex) {
			$.AEvents.mutex = false;
			$.AEvents.query.push(m);
			$.AEvents.mutex = true;
		} else {
			setTimeout($.AEvents.addMessage(m), AE_TIMEOUT_ADD);
		}
	},
	
	$.AEvents.start = function(m) { 
		$.AEvents.addMessage(m),
		$.AEvents.canSend = true; },
	$.AEvents.stop = function() { $.AEvents.canSend = false; },
	
	$.AEvents.messageLoop = function() {
		if(!$.AEvents.canSend || !$.AEvents.mutexSend) {
			setTimeout($.AEvents.messageLoop, $.AEvents.timeout);
			return;
		};
		
		$.AEvents.mutexSend = false;

		var query = [];
		
		if($.AEvents.mutex) {			
			$.AEvents.mutex = false;			
			query = $.AEvents.query;
			$.AEvents.query = [];
			$.AEvents.mutex = true;			
		}
			
		if(query.length == 0) 
			query.push({ action: 'ping' });
		
		$.AEvents.processMessages(query, function(mc) {
			if(mc == 0) $.AEvents.timeout += AE_TIMEOUT_D;
			if(mc > AE_MSG_MAX) { 
				$.AEvents.timeout -= AE_TIMEOUT_D; 
				if($.AEvents.timeout < AE_TIMEOUT_MIN) 
					$.AEvents.timeout = AE_TIMEOUT_MIN;
			}					
			setTimeout($.AEvents.messageLoop, $.AEvents.timeout);
			$.AEvents.mutexSend = true;
		});		
	},	
			
	$.AEvents.processMessages = function(q, cb) {
		if($.AEvents.testCallback) {
			var mp = $.AEvents.dispatchMessages($.AEvents.testCallback(q));
			if(cb) cb(mp);
		} else {
			$.post("/action", { m: $.toJSON(q) }, function(d) { 		 		
		 			if(d) {
		 				var mp = $.AEvents.dispatchMessages(d);
		 				if(cb) cb(mp);
		 			} else {
		 				if($.AEvents.errorCallback) $.AEvents.errorCallback();
		 			}
				}, "json")
		}
	},
		
	$.AEvents.dispatchMessages = function(p) {
		var k = 0;
		
		$(p).each(function(pi, pv) {
				if($.AEvents.map && pv && pv.action) 
					$($.AEvents.map[pv.action]).each(function(ei, ev) { 
							if(ev(pv)) k++; 
						});
			})
		return k;
	},
		
	/*
	 * 	Process events
	 */
	
	$.AEvents.bind = function(event, fn) {
		if($.isArray($.AEvents.map[event])) {
			if($.inArray(fn, $.AEvents.map[event]) == -1) {
				$.AEvents.map[event].push(fn);
			}
		} else {
			$.AEvents.map[event] = [fn];
		}
	}, 
	
	$.AEvents.detach = function(event, fn) {
		if($.isArray($.AEvents.map[event])) {
			var t = [];
			$($.AEvents.map[event]).each(function(i, v) {
				if(v != fn) t.push(v);
			})
			$.AEvents.map[event] = t;
		}
	}	
})(jQuery);

