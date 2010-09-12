/*
 * ViCo, test suite
 */
(function($){
	var isLogged = false;
	var	timeout = 0;
	
	$.vico._TEST = function(p) {
		var m = [];
		
		$(p).each(function(i, v) {
				if($.vico['_TEST_' + v.action]){
					var mi = $.vico['_TEST_' + v.action](v);
					if($.isArray(mi)) m = m.concat(m, mi);
					else if(mi) m.push(mi);
				}
			});				
		return m;
	}
	
	$.vico._TEST_ping = function(p) {
		timeout++;
		
		if(timeout == 16)
			return $.vico._login(p, function(d) { return({ action: 'exit', id: '0' }); });							
		else {
			if(timeout == 32)
				return $.vico._login(p, function(d) { return({
					action: 'enter',
					url: 'rtmp://91.151.179.160/rtmp/ch3',
					title: 'Головной офис',
					style: 'other',
					id: '4'
					}); });			
			else
				return $.vico._login(p, function(d) { return({ action: 'pong' }); });
		}
	}

	$.vico._TEST_init = function(p) {
		return $.vico._login(p, function(d) { 
				var m = [];				
				
				m.push({
					action: 'enter',
					url: 'rtmp://91.151.179.160/rtmp/max',
					title: 'Головной офис',
					style: 'lead',
					id: '0'
					});
				m.push({
					action: 'enter',
					url: 'rtmp://91.151.179.160/rtmp/ch3',
					title: 'Филиал 1',
					style: 'lead',
					id: '1'
					});
				m.push({
					action: 'enter',
					url: 'rtmp://91.151.179.160/rtmp/max',
					title: 'Филиал 2',
					style: 'other',
					id: '2'
					});				
				return m;
			});
	}

	$.vico._TEST_chat = function(p) {
		return $.vico._login(p, function(d) { 
				return {
					action: 'chat',
					title: 'Upcase robot',
					text: d.text.toUpperCase(),
					id: '0'
					};
			});
	}

	
	$.vico._TEST_auth = function(p) {
		if(!isLogged) {
			if(p.login ==  'max' && p.password == '123') {
				isLogged = true;
				return({ 
					action: 'authok', 
					sid: '00123456789abcdef', 
					title: 'Течт',
					server: 'rtmp://91.151.179.160/rtmp/',
					stream: 'max' });
			} else if(p.login ==  'ch3' && p.password == '123') {
				isLogged = true;
				return({ 
					action: 'authok', 
					sid: '00123456789abcdef', 
					title: 'Течт',
					server: 'rtmp://91.151.179.160/rtmp/',
					stream: 'ch3' });
			} else
				return({ action: 'autherror' });						
		} else {					
			return({ action: 'autherror' });						
		}
	}

	$.vico._login = function(p, cb) {
		if(!isLogged) return({ action: 'noauth' });
		else return cb(p);
	}	
	
})(jQuery);

$(document).ready(function() { $.vico(); });