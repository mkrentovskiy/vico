/*
	FormCheck for jQuery
	(c) ReLabs, 2008
	http://relabs.ru/
*/ 

(function($) {	
 	$.FormCheck = function() {
		// online 
		$('form[vmethod="online"]').submit(function() {
			return $.FormCheck.validateForm(this);
		});
		
		$.FormCheck.avalTypes = new Array('input[type=text]', 'input[type=password]', 
			'input[type=radio]', 'input[type=checkbox]', 'textarea', 'select');
		
		jQuery.each($.FormCheck.avalTypes, function (n, i) {
			$('form[vmethod=online] ' + i).focus(function() {
				return $.FormCheck.onProcess(this);
			});
		
			$('form[vmethod=online] ' + i).blur(function() {
				return $.FormCheck.onValidate(this);
			});
		});
		
		// onsubmit
		$('form[vmethod="onsubmit"]').submit(function() {
			return $.FormCheck.validateForm(this);
		});
				
	}, 
	
	$.FormCheck.avalTypes = null,

	$.FormCheck.onProcess = function(element) {
		$.FormCheck.resetState(element);
	},

	$.FormCheck.onValidate = function(element) {
		var r = $.FormCheck.validate(element);
		if(!element.waitForValadation) {
			if(r)
				$.FormCheck.setState(element, 'right');
			else
				$.FormCheck.setState(element, 'wrong');
		} 
	},

	$.FormCheck.validate = function(element) {
		var valid = false;
		element.waitForValadation = true;
		
		switch($(element).attr('vmethod')) {
			case('optional') : {
				valid = $.FormCheck.check(element, $(element).attr('vparam'), true);
				element.waitForValadation = false;
				break;
			}
			case('pattern') : {
				valid = $.FormCheck.check(element, $(element).attr('vparam'), false);
				element.waitForValadation = false;
				break;
			}
			case('callback') : {
				$.FormCheck.setState(element, 'checking');
				$.ajaxSetup({ async: false });
				eval('valid = ' + $(element).attr('vparam') + '(element);');
				$.ajaxSetup({ async: true });
				break;
			}
			default: {
				valid = true;
				element.waitForValadation = false;
				break;
			} 
		}
		return valid;
	},
	
	$.FormCheck.validateForm = function(form) {
		var valid = true;
		
		jQuery.each(form.elements, function(n, e) {
			var s1 = e.nodeName.toLowerCase() + "[type=" + e.type.toLowerCase() + "]";
			var s2 = e.nodeName.toLowerCase(); // textarea also has type "textarea" o_0
			
			if(jQuery.inArray(s1, $.FormCheck.avalTypes) != -1 ||
				jQuery.inArray(s2, $.FormCheck.avalTypes) != -1) {
					var ve = $.FormCheck.validate(e);
					valid = valid && ve;	
					
					if(ve) $.FormCheck.setState(e, 'right');
					else $.FormCheck.setState(e, 'wrong');
			}		
			
		});
		return valid;
	},
	
	$.FormCheck.setState = function(element, state) {
		$(element).removeClass('vchecking');
		$(element).removeClass('vwrong');
		$(element).removeClass('vright');
		
		$(element).addClass('v' + state);	
		
		if(state == 'wrong') {
			$.FormCheck.showNotice(element, $(element).attr('vnotice'));
		}
	},

	$.FormCheck.resetState = function(element) {
		if(!element.waitForValadation) {
			$(element).removeClass('vwrong');
			$(element).removeClass('vright');
			$(element).removeClass('vchecking');
		}
	},
	
	$.FormCheck.showNotice = function(element, notice) {
		var nid = 'notice-' + element.id;

		if(!notice || notice.length == 0) {
			notice = "It's not the valid value";			
		}
		
		if($("#" + nid).length == 0) {
			$("body").append("<div id='" + nid + "' class='vnotice'>" + 
				notice + "</div>");
		}
		$("#" + nid).fadeIn();		
				
		var o = $(element).offset();
		var w = $(element).width();
		var h = $(element).height();
		var b =  $("#" + nid).height();
		
		var flip = !(w - 10 + o.left + b < w);
		$("#" + nid).css('top', (o.top - 10 + h) + 'px');
		$("#" + nid).css('left', (o.left + w - (flip ? b : 0) - 25) + 'px');

		$("#" + nid).bind('click', function() { $.FormCheck.hideNotice(nid); });
		setTimeout(function() { $.FormCheck.hideNotice(nid); }, 5000);
		$(element).bind('focus', function() { $.FormCheck.hideNotice(nid); });		
	},

	$.FormCheck.hideNotice = function(nid) {
		$("#" + nid).fadeOut();
	},
	
	$.FormCheck.check = function(element, param, init) {
		var result = init;

		switch(element.type) {
			//
			// text elements
			// param :=  [ string | integer | float | url | mail | {RegExp}];
			//		
			case 'text':
			case 'password':
			case 'textarea':
				if(element.value.length == 0) return result;					
				if(param) {
					switch(param) {
						case 'string':
							result = (element.value.length > 0);
							break;
	
						case 'integer':
							var re = new RegExp('^[0-9]+$', "g");
							result = re.test(element.value);
							break;

						case 'float':
							var re = new RegExp("^[1-9]?[0-9]+(\\.[0-9]+)?$", "g");
							result = re.test(element.value);
							break;

						case 'url':
							var re = new RegExp();
						    re.compile("^[A-Za-z]+://[A-Za-z0-9-_%&\?\/.=]+$"); 
							result = re.test(element.value);
							break;

						case 'mail':
							var re = new RegExp('^[0-9a-z\._\-]+@[0-9a-z\-]+\\..+$', "g");
							result = re.test(element.value.toLowerCase());
							break;

						default:	
							var re = new RegExp(param, "g");
							result = re.test(element.value);
							break;
					}
				}
				break;
			//
			// groups
			// param :=  [min] : [max];
			//		
			case 'radio':
			case 'checkbox':
				if(param.length > 1) {
					var ps = param.split(/\:/);
					var mmin = ps[0];
					var mmax = ps[1];

					if(mmin || mmax) {
						var i = 0;
							
						jQuery.each($("input[name=" + element.name + "]"), function (n, obj) {
								if(obj.checked) i++;
							});
						result =  result || ( i >= mmin && i <= mmax); 
					}
				} else {
					result = true;					
				}
				break;
			
			//
			// another elements
			//		
			case 'select-one':
			case 'select-multiple':
				if(element.selectedIndex) result = true;
				break;
				
			case 'file':
			case 'image':
			case 'button':
			case 'submit':
			case 'reset':
			default:
				break;
		}
		return result;
	}
	
			
})(jQuery);

// Init lib
$(document).ready(function() { $.FormCheck(); });

// From jQuery.Form plugin

$.fn.clearForm = function() {
	return this.each(function() {
    	var type = this.type, tag = this.tagName.toLowerCase();
      if (tag == 'form')
        return $(':input',this).clearForm();
      if (type == 'text' || type == 'password' || tag == 'textarea') {
        this.value = '';
      } else if (type == 'checkbox' || type == 'radio')
        this.checked = false;
      else if (tag == 'select')
        this.selectedIndex = -1;
      
      $(this).removeClass('vwrong');
      $(this).removeClass('vright');
      $(this).removeClass('vchecking');
    });
};	

	