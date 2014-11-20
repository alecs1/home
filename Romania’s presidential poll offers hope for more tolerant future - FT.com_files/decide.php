/* a103 - Mon Aug 11 10:13:05 EDT 2014 */
(function(){
var d=document,w=window,s_id="SafeCount_Layer",b;

// Pass the location of ad served page thru to GF or dynamiclink.js.php
// pass protocol and uri separately or it won't get thru gatefile.
var DL_loc = "&loc=aHR0cDovL3d3dy5mdC5jb20vaW50bC9jbXMvcy8wLzRkN2UzNWE4LTZmNDctMTFlNC1iNTBmLTAwMTQ0ZmVhYmRjMC5odG1s";

// If inside of Friendly iFrame
// pass iFrame window name and id
if (typeof inDapIF != 'undefined' &&  inDapIF == true) {
	DL_loc += "&fif=1&fifid="+w.frameElement.id+"&fifname="+w.frameElement.name;
}
function z(){
var s;
if (d.URL.toLowerCase().indexOf('3dgamers.com')==-1){
	s=d.getElementById(s_id);
	if (!s) {	
		s=d.createElement('script');
		s.id=s_id;
		s.src="//amch.questionmarket.com/adscgen/dynamiclink.js.php?sub=amch&type=chartbeat&survey_num=1146465&site=2&code=1154160&p=1&protocol=http&lang=&noiframe=1"+DL_loc;
		d.getElementsByTagName('head').item(0).appendChild(s);
	}
}
}setTimeout(z, 2000);
}());
