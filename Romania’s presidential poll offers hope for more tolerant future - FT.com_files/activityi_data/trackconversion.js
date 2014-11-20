/*sample javascript to parse the data*/
var drawbridge_img_tag = "<div style=\"display:inline;\" " +
		"><img height=\"1\" width=\"1\" " +
		"style=\"border-style:none;\" alt=\"\" " +
		"src=\"" + location.protocol + "//api.adsymptotic.com/api/s/trackconversion?_pid="+drawbridge_partner_id
		+ "&_psign=" + drawbridge_partner_sign + "&_aid=" + drawbridge_app_id + "&_lbl=" +
		drawbridge_label + "&_rand=" + Math.random();

if(window.drawbridge_it)
  drawbridge_img_tag += "&_it=" + drawbridge_it;
  
drawbridge_img_tag += "\"/></div>";

document.write(drawbridge_img_tag);
