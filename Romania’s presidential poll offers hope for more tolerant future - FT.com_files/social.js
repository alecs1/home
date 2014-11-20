window.nut=function(){var e=document,t="firstChild",n="nextSibling",r="getElementsByClassName",i="length",s=function(t,n){var r=e.getElementById(t);return r?[r]:[]},o=function(e,r){var s=r[t],u=[],a;if(s)do s.nodeType==1&&(s.className&&s.className.match("\\b"+e+"\\b")&&u.push(s),(a=o(e,s))[i]&&(u=u.concat(a)));while(s=s[n]);return u},u=function(e,t){return t[r]?t[r](e):o(e,t)},a=function(e,t){return t.getElementsByTagName(e)};return function(t,n){n||(n=e),typeof n=="object"&&n.pop&&(n=n[0]);var r,o,f,l,c=[],h,p,d,v,m,g,y;t=t.split(","),m=-1;while(f=t[++m])t[m]=f.split(/\s+/);h=t[i];while(h){r=[n],p=-1,d=t[--h][i];while(++p<d)if(f=t[h][p]){f.charAt(0)=="#"?(f=f.substr(1),y=s):f.charAt(0)=="."?(f=f.substr(1),y=u):y=a,o=[],v=-1;while(r[++v]){l=y(f,r[v]),m=-1,g=l[i];while(++m<g)o.push(l[m])}r=o}c=c.concat(r)}return c}}();
!function(e){typeof define=="function"&&define.amd?define(e):window.onDomReady=e()}(function(){"use strict";function d(e){if(!h){if(!t.body)return m(d);h=!0;while(e=p.shift())m(e)}}function v(){l?(t.removeEventListener(a,v,r),d()):t[s]===i&&(t.detachEvent(f,v),d())}function m(e,t){setTimeout(e,+t>=0?t:1)}function y(e){h?m(e):p.push(e)}var e=window,t=e.document,n=t.documentElement,r=!1,i="complete",s="readyState",o="attachEvent",u="addEventListener",a="DOMContentLoaded",f="onreadystatechange",l=u in t,c=r,h=r,p=[];if(t[s]===i)m(d);else if(l)t[u](a,v,r),e[u]("load",d,r);else{t[o](f,v),e[o]("onload",d);try{c=e.frameElement==null&&n}catch(g){}c&&c.doScroll&&function b(){if(!h){try{c.doScroll("left")}catch(e){return m(b,50)}d()}}()}return y.version="1.2",y});
function microAjax(B,A){this.bindFunction=function(E,D){return function(){return E.apply(D,[D])}};this.stateChange=function(D){if(this.request.readyState==4){this.callbackFunction(this.request.responseText)}};this.getRequest=function(){if(window.ActiveXObject){return new ActiveXObject("Microsoft.XMLHTTP")}else{if(window.XMLHttpRequest){return new XMLHttpRequest()}}return false};this.postBody=(arguments[2]||"");this.callbackFunction=A;this.url=B;this.request=this.getRequest();if(this.request){var C=this.request;C.onreadystatechange=this.bindFunction(this.stateChange,this);if(this.postBody!==""){C.open("POST",B,true);C.setRequestHeader("X-Requested-With","XMLHttpRequest");C.setRequestHeader("Content-type","application/x-www-form-urlencoded");C.setRequestHeader("Connection","close")}else{C.open("GET",B,true)}C.send(this.postBody)}};
var FT = FT || {};

FT.social = (function () {

    var config = {
            registrationBaseUrl: 'https://registration.ft.com',
            registrationAssetsUrl: 'https://registration.ft.com',
            appId: 'abdmmlbjpegmnihfjcem',
            appUrl: 'https://slogin.ft.com',
            engageEnvironmentName: 'slogin.ft.com'
        },

        errorMessages = {
            INVALID_CLIENT_STATE:           {title: 'Your FT.com session has expired', body: 'Please login again to link your account.', loginRequired: true},
            NO_ACTIVE_SESSION:              {title: 'Your FT.com session has expired', body: 'Please login again to link your account.', loginRequired: true},
            SOCIAL_ACCOUNT_ALREADY_LINKED:  {title: 'This account is already linked to another FT.com account', body: 'You must unlink from that account to be able to link to this one.', loginRequired: false},
            NATIVE_ACCOUNT_ALREADY_LINKED:  {title: 'This FT.com account is already linked to another twitter account', body: 'You must unlink your current twitter account to be able to link to this one.', loginRequired: false}
        },

        janrainEngageScriptSource;

    var social = {

        errorMessages: errorMessages,

        init: function init(tokenUrl) {
            if (typeof window.janrain !== 'object') window.janrain = {};
            if (typeof window.janrain.settings !== 'object') window.janrain.settings = {};

            janrain.settings.tokenUrl = tokenUrl;
            janrain.settings.custom = true;
            janrain.settings.appId = config.appId;
            janrain.settings.appUrl = config.appUrl;
            janrain.settings.providers = ["twitter"];

            if (document.location.protocol === 'https:') {
                janrainEngageScriptSource = 'https://rpxnow.com/js/lib/' + config.engageEnvironmentName + '/engage.js';
            } else {
                janrainEngageScriptSource = 'http://widget-cdn.rpxnow.com/js/lib/' + config.engageEnvironmentName + '/engage.js';
            }

        },

        addMarkupToElement: function(element, markup) {
            nut(element)[0].innerHTML = "_" + markup;
            nut(element)[0].removeChild(nut(element)[0].firstChild);
        },

        loadSocialWidget: function loadSocialWidget(parameters) {
            window.janrainWidgetOnload = function () {
                janrain.ready = true;
                var linkButtonsUrl = config.registrationBaseUrl + '/social/link-buttons?target=' + parameters.target;
                microAjax(linkButtonsUrl, function (data) {
                    if (data.length > 0) {

                        social.addMarkupToElement(parameters.element, data);

                        var twitterButtonElement = document.getElementById("linkTwitterButton");
                        if (twitterButtonElement) {
                            janrain.engage.signin.setProviderFlow(twitterButtonElement, "twitter");
                            twitterButtonElement.click = function (e) {
                                janrain.engage.signin.triggerFlow("twitter");
                            };
                        }
                    }
                });
            };

            social.init(config.registrationBaseUrl + "/social/link?successUrl=" + encodeURIComponent(parameters.successUrl));
            loadJanrainScript(janrainEngageScriptSource);

            onDomReady(function() {
                social.displayErrorIfNeeded(parameters);
                trackLinkStatusChange();
            });
        },

        loadLoginWidget: function loadLoginWidget(parameters) {
            window.janrainWidgetOnload = function() {
                janrain.ready = true;
                var html = social.getLoginButtonsMarkup();
                if (parameters.element) {
                    nut(parameters.element)[0].innerHTML = html;
                } else if (parameters.appendInElement) {
                    var socialDiv = document.createElement('div');
                    socialDiv.innerHTML = html;
                    nut(parameters.appendInElement)[0].appendChild(socialDiv);
                }

                var twitterButtonElement = document.getElementById("loginTwitterButton");
                janrain.engage.signin.setProviderFlow(twitterButtonElement, "twitter");
                twitterButtonElement.click = function (e) {
                    janrain.engage.signin.triggerFlow("twitter");
                };
            };

            if ((elementExists(parameters.element) ||
                 elementExists(parameters.appendInElement)) &&
                    !elementExists("#loginTwitterButton")) {
                social.init(config.registrationBaseUrl + "/social/login?successUrl=" + encodeURIComponent(parameters.successUrl) + "&unsuccessfulUrl=" + encodeURIComponent(parameters.unsuccessfulUrl));
                loadJanrainScript(janrainEngageScriptSource);

                onDomReady(function() {
                    trackLoginSuccess();
                });
            }

            if (parameters.registrationAssetsUrl) {
                config.registrationAssetsUrl = parameters.registrationAssetsUrl;
            }
        },

        getLoginButtonsMarkup: function getLoginButtonsMarkup() {
            var twitterLogoPath = config.registrationAssetsUrl + "/social/assets/images/logo-twitter.png";

            var html = '';
            html += '<div class="social-login">';
            html += '<a id="loginTwitterButton" title="Twitter" ';
            html += ('style="cursor: pointer; color: #ffffff; background: #77c4e0 url(\'' + twitterLogoPath + '\') no-repeat 10px; -moz-border-radius: 3px; border-radius: 3px; display: block; width: 229px; height: 36px; line-height: 36px; font-size: 14px; text-align: left; text-indent: 50px; margin: auto; margin-top: 1em;" ');
            html += 'href="javascript:void(0);">';
            html += 'Sign in with Twitter</a>';
            html += '</div>';

            return html;
        },

        unlinkSocialAccount: function unlinkSocialAccount() {
            var form = document.createElement("form");
            form.setAttribute("method", "post");
            form.setAttribute("action", config.registrationBaseUrl + "/social/unlink?successUrl=" + document.URL);
            document.body.appendChild(form);
            form.submit();
        },

        displayErrorIfNeeded: function displayErrorIfNeeded(parameters) {
            var errorCookieValue = getCookieAndExpire('FT_LINK_ERROR');
            if (errorCookieValue) {
                var error = retrieveError(errorCookieValue);
                FT.error.displayErrorOverlay(error.title, error.body, error.loginRequired, parameters.redirectUrlForError);
            }
        },

        getLocationParameter: function getLocationParameter() {
            var location = getURLParameter("location") || getURLParameter("_i_location");

            if(location && location.match(/^http%3A.*$/)){
                return decodeURIComponent(location);
            }
            else{
                return location;
            }
        }

    };

    // private methods

    function elementExists(selector) {
        return selector && nut(selector).length > 0;
    }

    function expireCookie(name) {
        var date = new Date();
        date.setTime(date.getTime()+(-1*24*60*60*1000));
        var expires = "; expires="+date.toGMTString();
        document.cookie = name+"="+expires+"; path=/; host=.ft.com; domain=.ft.com";
    }

    function loadJanrainScript(scriptSource) {
        var e = document.createElement('script');
        e.type = 'text/javascript';
        e.id = 'janrainWidgets';
        e.src = scriptSource;
        var s = document.getElementsByTagName('script')[0];
        s.parentNode.insertBefore(e, s);
    }

    function retrieveError(errorEnum) {
        var error = {
            title:'We have encountered an unexpected error.',
            body:'Please try again later.',
            loginRequired:false
        };

        if (errorEnum in errorMessages) {
            error.title = errorMessages[errorEnum].title;
            error.body = errorMessages[errorEnum].body;
            error.loginRequired = errorMessages[errorEnum].loginRequired;
        }

        return error;
    }

    function getCookieAndExpire(c_name) {
        var i,x,y,ARRcookies=document.cookie.split(";");
        for (i=0;i<ARRcookies.length;i++) {
            x=ARRcookies[i].substr(0,ARRcookies[i].indexOf("="));
            y=ARRcookies[i].substr(ARRcookies[i].indexOf("=")+1);
            x=x.replace(/^\s+|\s+$/g,"");
            if (x==c_name) {
                expireCookie(c_name);
                return unescape(y);
            }
        }
    }

    function getURLParameter(name) {
        return decodeURIComponent((new RegExp('[?|&]' + name + '=' + '([^&;]+?)(&|#|;|$)').exec(location.search)||[,""])[1].replace(/\+/g, '%20'))||null;
    }

    function trackEvent(event, trackFn) {
        var trackingCookieValue = getCookieAndExpire(event);
        if (trackingCookieValue && typeof trackFn === 'function') {
            trackFn(trackingCookieValue);
        }
    }

    function trackLoginSuccess() {
        trackEvent('SOCIAL_LOGIN_SUCCESS', function(accountType){
            FT.analytics && FT.analytics.siteIntelligence && FT.analytics.siteIntelligence.trackData("login_success=" + accountType);
        });
    }

    function trackLinkStatusChange() {
        trackLinkingSuccess();
        trackUnlinkingSuccess();
    }

    function trackLinkingSuccess() {
        trackEvent('SOCIAL_LINKING_SUCCESS', function(accountType){
            FT.analytics && FT.analytics.siteIntelligence && FT.analytics.siteIntelligence.sendAdditionalTracer("/account_linked", accountType + "=true")
            || typeof SiSendAdditionalTracer === 'function' && SiSendAdditionalTracer("/account_linked", accountType + "=true");
        });
    }

    function trackUnlinkingSuccess() {
        trackEvent('SOCIAL_UNLINKING_SUCCESS', function(accountType){
            FT.analytics && FT.analytics.siteIntelligence && FT.analytics.siteIntelligence.sendAdditionalTracer("/account_linked", accountType + "=false")
            || typeof SiSendAdditionalTracer === 'function' && SiSendAdditionalTracer("/account_linked", accountType + "=false");
        });
    }

    return social;

}());
