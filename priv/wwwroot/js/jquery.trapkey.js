/*
 *
 * Copyright (c) 2009 C. F., Wong (<a href="http://cloudgen.w0ng.hk">Cloudgen Examplet Store</a>)
 * Licensed under the MIT License:
 * http://www.opensource.org/licenses/mit-license.php
 * 
 * See details in: <a href="http://cloudgen.w0ng.hk/javascript/javascript.php">Javascript Examplet</a>
 *
 */
(function($){
	var Kb_keys=new Array(),
		Kb_lastkey=-1,
		Kb_keystrapped=0,
		Kb_lastcode=-1;
	$.trapKey=function(akey){
		return (new TrappedKey(akey.toUpperCase()));
	};
	$.clearAllTrapedKey=function(akey){
		var n;
		for(n in Kb_keys)Kb_keys[n]=null
	};
	function kp(wch,state){
		if(Kb_keys[wch]){
			Kb_keys[wch].pressed=state;
			Kb_lastkey=Kb_keys[wch];
			if(state&&Kb_keys[wch].event){
				Kb_keys[wch].event()
			}else return false;
		}
	}
	function TrappedKey(newkey){
		if(Kb_keystrapped==0){
			if(window.event){
				document.body.onkeydown=function(){
					Kb_lastcode=window.event.keyCode;
					kp(window.event.keyCode,true)
				};
				document.body.onkeyup=function(){
					kp(window.event.keyCode,false)
				};
			}else {
				document.onkeydown=function(evt){
					Kb_lastcode=evt.which;
					kp(evt.which,true)
				};
				document.onkeyup=function(evt){
					kp(evt.which,false)
				};
			} 
		}
		if(newkey.length>1){
			var c=0;
			switch(newkey){
				case 'SHIFT':
				c=16;break;
				case 'CTRL':
				c=17;break;
				case 'UP':
				c=38;break;
				case 'DOWN':
				c=40;break;
				case 'INSERT':
				c=45;break;
				case 'LEFT':
				c=37;break;
				case 'RIGHT':
				c=39;break;
				case 'ESC':
				c=27;break;
				default:
				c=0
			}
			this.code=c;
		}else
			this.code=newkey.charCodeAt(0);
		this.pressed=false;
		this.event=null;
		Kb_keys[this.code]=this;
		if(newkey.length==1&&newkey.toUpperCase()!=newkey.toLowerCase()){
			this.code=newkey.toLowerCase().charCodeAt(0);
			Kb_keys[this.code]=this;
		}
		Kb_keystrapped++;
		return this;
	}
	TrappedKey.prototype.setEvent=function(x){
		this.event=x;
		return this
	};
	TrappedKey.prototype.clearEvent=function(x){
		this.event=null;
		return this
	};
})(jQuery);