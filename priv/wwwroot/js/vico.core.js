/*
 * ViCo - Videoconferencing JS control
 */
(function($){
	var LOADING = "<img src='i/loading.gif' alt='loading...' title='loading...'/>";
		
	var isLogged = false;
	var user = {};
	
	var isShow = false;	
	var videoType = "web";
	
	$.vico = function() {
		$('#emsg').ajaxError(function() { $.vico.pipeError(); });
		
		$.vico.showPanel(videoType);		
		
		if($.vico._TEST) $.AEvents($.vico._TEST, $.vico.pipeError);
		else $.AEvents(null, $.vico.pipeError);
		
		$.AEvents.bind("noauth", $.vico.noAuth);
		$.AEvents.bind("authok", $.vico.authPassed);
		$.AEvents.bind("init", $.vico.reinit);
		$.AEvents.bind("autherror", $.vico.authError);		

		$.AEvents.bind("connect", $.vico.roomConnect);		
		$.AEvents.bind("disconnect", $.vico.roomDisconnect);		

		$.AEvents.bind("enter", $.vico.roomEnter);		
		$.AEvents.bind("exit", $.vico.roomExit);		

		$.AEvents.bind("chat", $.vico.chat);
		
		$.AEvents.start({ action: 'init' });
	};		
	
	$.vico.login = function() {
		if($.FormCheck.validateForm($("#loginform")[0])) {
			$("#loginbutton").attr("disabled", "disabled");
			$("#login_result").html(LOADING);	
			$.AEvents.addMessage({ 
				action: 'auth', 
				login: $("#i_login").val(), 
				password: $("#i_password").val() });
		}
	};
	
	$.vico.showPanel = function(p) {
		if(isShow) return;
		var pannelList = new Array("web", "app", "cam");
			
		$(pannelList).each(function(i, pi) {
				if((!$("#vbox_" + pi).hasClass("no") && pi != p) || 
						(pi == p && $("#vbox_" + p).hasClass("no")))					
					$("#vbox_" + pi).toggleClass("no"); 
			}); 
		videoType = p;
	};
	
	$.vico.onStartStop = function() {
		if(!isLogged) return;
		
		if(videoType == "cam") 
			if(!$.FormCheck.validateForm($("#h264_stream_form")[0])) return;
		
		if(!isShow) {
			$.AEvents.addMessage({ 
				action: 'start', 
				type: videoType, 
				url: $("#h264_stream_form").val() });
			$("#bstart").removeClass("bstart").addClass("bstop").html("Отключить трансляцию");
		} else {
			$.AEvents.addMessage({ action: 'stop' });
			$("#bstart").removeClass("bstop").addClass("bstart").html("Запустить трансляцию");			
		}
		isShow = !isShow;
	};
	
	$.vico.shCtlPanel = function(sh) {
		$("#ctlpanelmin").toggleClass("no");
		$("#myvideo").toggleClass("no");
		
		if(sh) {
			$("#vroom").css("padding", "0px 480px 0px 0px");
			$("#ctlpanelmax").animate({'margin-left': -480}, 1000);
			$("#myvideoitem").html('');
		} else { 
			$("#vroom").css("padding", "0px");
			$("#ctlpanelmax").animate({'margin-left': 0}, 1000);
			$("#myvideoitem").flash({ id: "#myvideoitem_player", 
				swf: "swf/view_other.swf",
				width: $("#myvideoitem").width(), 
				height: $("#myvideoitem").height(), 
				params: { allowfullscreen: true, wmode: "transparent" },
				flashvars: { url: user.server + user.stream } });
		}
	};	

	$.vico.sendToChat = function() {
		if($.FormCheck.validateForm($("#chatform")[0])) {
			var text = $("#ichat").val();
			
			$.vico.addToChat(user.title, text, 'me');
			$.AEvents.addMessage({ 
				action: 'chat', 
				text: text });
			$("#chatform").clearForm();			
		}	
		return false;
	};
		
	$.vico.addToChat = function(nick, text, style) {
		var re_url = new RegExp("[A-Za-z]+://[A-Za-z0-9-_%&\?\/.=]+", "g");
		var re_mail = new RegExp("[0-9a-z\._\-]+@[0-9a-z\-]+\\..+", "g");
	    var re_n = new RegExp("\n", "g");
	    var re_lt = new RegExp("<", "g");
	    var re_gt = new RegExp(">", "g");
	    var re_amp = new RegExp("&", "g");	    
		
		text = text.replace(re_amp, "&amp;");
		text = text.replace(re_lt, "&lt;");
		text = text.replace(re_gt, "&gt;");
		text = text.replace(re_url, function(v) { 
			return "<a href='" + v + "' target='_blank'>" + v + "</a>"; });
		text = text.replace(re_mail, function(v) { 
			return "<a href='mailto:" + v + "'>" + v + "</a>"; });
		text = text.replace(re_n, "<br/>");
		
		$("#chatroom").prepend($.nano($("#_chat_phrase").html(), { 
				nick: nick, 
				text: text, 
				style: style}));
	};
	
	$.vico.pipeError = function() {
		$.cookie("SID", null);
		$('#emsg').text('We got AJAX error. Sorry!').fadeIn(500, function() {
			setTimeout(function() { $('#emsg').fadeOut(500, function() {
					setTimeout(function() { window.location.href = '/'; }, 10000)
				}); }, 10000)
		});
		$("#overlay").removeClass("no");		
	}	
	
	/*
	 * 	Messages callback
	 */
	
	$.vico.noAuth = function(p) {
		isLogged = false;
		
		if($("#loginbox").hasClass("no")) {
			$("#overlay").removeClass("no");		
			$("#login_result").html('');			
			$("#loginbox").removeClass("no");
			$("#ichat").removeAttr("autofocus");
			$("#i_login").focus();
			$("#loginbutton").removeAttr("disabled");			
		}
		
		return true;
	}

	$.vico.authPassed = function(p) {
		$.AEvents.addMessage({ action: 'init' });
		return true;
	}
	
	$.vico.reinit = function(p) {
		isLogged = true;
		user = p;
		
		$("#chatform").clearForm();
		$("#chatroom").html('');
		$("#ichat").attr("autofocus","autofocus").focus();

		$("#vroom_item div.headline").detach();
		$("#vroom_item div.others").detach();
				
		$("#fout").html('').flash({ id: "fout_app", 
				swf: "swf/broadcast.swf",
				width: 352, 
				height: 388, 
				params: { allowfullscreen: true, wmode: "transparent" },
				flashvars: { server: p.server, stream: p.stream } });
		$("#vlink").html(p.server + p.stream );

		$("#overlay").addClass("no");
		$("#loginbox").addClass("no");
			
		return true;
	}
	
	$.vico.authError = function(p) {
		$("#login_result").html("Неправильная пара логин-пароль");	
		$("#loginform").clearForm();
		$("#loginbutton").removeAttr("disabled");			
	
		return true;
	}

	$.vico.roomConnect = function(p) {
		if(p.id != user.login) {
			if(!$("#r_" + p.id).length) $("#roaster").append($.nano($("#_roaster_user_pattern").html(), p));
			$("#r_" + p.id).fadeIn();			
		}
		return true;
	}	

	$.vico.roomDisconnect = function(p) {
		$("#r_" + p.id).fadeOut();
		return true;
	}	
	
	$.vico.roomEnter = function(p) {
		$("#v_" + p.id).detach();
		$("#vroom_item").append($.nano($("#_" + p.style + "_video_pattern").html(), p));
		
		$("#vf_" + p.id).flash({ id: "vif_" + p.id, 
			swf: "swf/view_" + p.style +".swf",
			width: $("#vf_" + p.id).width(), 
			height: $("#vf_" + p.id).height(), 
			params: { allowfullscreen: true, wmode: "transparent" },
			flashvars: { url: p.url } });
		return true;
	}

	$.vico.roomExit = function(p) {
		$("#v_" + p.id).remove();
		return true;
	}

	$.vico.chat = function(p) {
		if(p.title == user.title)
			$.vico.addToChat(p.title, p.text, 'me');		
		else
			$.vico.addToChat(p.title, p.text, 'notme');
		return true;
	}
	
})(jQuery);

$(document).ready(function() { $.vico(); });