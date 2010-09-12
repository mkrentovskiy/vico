/*
 * ViCo control
 */
(function($){
	var LOADING = "<img src='i/loading.gif' alt='loading...' title='loading...'/>";
		
	$.vico = {};
	$.vico.control = function() {
		$('#emsg').ajaxError(function() { $.vico.pipeError(); });
		$("#result").html(LOADING);
		
		$.cookie("SID", null);
		
		$.AEvents(null, $.vico.pipeError);		
		$.AEvents.bind("userslist", $.vico.usersList);		
		$.AEvents.start({ action: 'userslist' });
	};		

	$.vico.pipeError = function() {
		$('#emsg').text('We got AJAX error. Sorry!').fadeIn(500, function() {
			setTimeout(function() { $('#emsg').fadeOut(500); }, 10000)
		});
		$("#overlay").removeClass("no");		
	};		
	
	$.vico.usersList = function(p) {
		$("#result").html('');		
		$(p.users).each(function(i, v) {
			$("#userslist").append($.nano($("#_user_pattern").html(), v));
		});
	};
	
	$.vico.addUser = function() {
		if(!$.FormCheck.validateForm($("#secret_form")[0])) return;

		if($.FormCheck.validateForm($("#add_user_form")[0])) {
			$("#result").html(LOADING);
			$("#userslist li").detach();
			
			$.AEvents.addMessage({ 
				action: 'adduser',
				key: $("#i_secret").val(),
				login: $("#i_login").val(),
				password: $("#i_password").val(),
				title: $("#i_title").val(),
				group: $("#i_group").val()
			});			
			$("#add_user_form").clearForm();
		}		
	}

	$.vico.deleteUser = function(login) {
		if(!$.FormCheck.validateForm($("#secret_form")[0])) return;
		
		if(confirm("Действительно удалить этого пользователя?")) {
			$("#result").html(LOADING);
			$("#userslist li").detach();

			$.AEvents.addMessage({ 
				action: 'deleteuser',
				key: $("#i_secret").val(),
				login: login
			});
		}
	}
	
})(jQuery);

$(document).ready(function() { $.vico.control(); });