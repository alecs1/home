var FT = FT || {};

FT.guestpass = (function () {

    var config = {
            guestPassBaseUrl: 'https://registration.ft.com/guestpass'
        },

        disableSendButton = false,

        guestpass = {

            config: config,

            loadGuestPassWidget: function loadGuestPassWidget() {
                if (isAllowedToSeeGuestPass()) {

                    addGuestPassButtonToButtonsList();
                    addGuestPassEmptyOverlay();

                    $(".guestPassButton").click(function() {
                        $.ajax({
                            url: config.guestPassBaseUrl + '/create-dialog',
                            type: "GET",
                            dataType: "jsonp",
                            jsonpCallback: "dialogCallback",
                            cache: false,
                            success: function(data) {
                                disableSendButton = false;
                                fillGuestPassOverlay(data.html);
                                openGuestPassOverlay();
                                initializeForm();
                                trackGuestPassClicked();
                            }
                        });
                    });
                }
            },

            trackGuestPassRedeemed: function trackGuestPassRedeemed() {
                var redeemedGuestPassId = getCookieAndExpire("GP_REDEEMED");
                redeemedGuestPassId && FT.analytics && FT.analytics.siteIntelligence && FT.analytics.siteIntelligence.trackData('guestpass=' + redeemedGuestPassId);
            }
        };

        // private functions

        function isAllowedToSeeGuestPass() {
            return isLoggedIn() && isASusbscriber();
        }

        function isLoggedIn() {
            return getCookie('FT_U');
        }

        function isASusbscriber() {
            var AYSC = getCookie('AYSC');
            if (!AYSC) { return false; }
            var products = AYSC.match(/_22([^_]*)/);
            return products && products.length > 1 && /P[123]/.test(products[1]);
        }

        function getCookie(c_name) {
            var i,x,y,ARRcookies=document.cookie.split(";");
            for (i=0;i<ARRcookies.length;i++) {
                x=ARRcookies[i].substr(0,ARRcookies[i].indexOf("="));
                y=ARRcookies[i].substr(ARRcookies[i].indexOf("=")+1);
                x=x.replace(/^\s+|\s+$/g,"");
                if (x==c_name) {
                    return unescape(y);
                }
            }
        }

        function getCookieAndExpire(c_name) {
            var i,x,y,ARRcookies=document.cookie.split(";");
            for (i=0;i<ARRcookies.length;i++) {
                x=ARRcookies[i].substr(0,ARRcookies[i].indexOf("="));
                y=ARRcookies[i].substr(ARRcookies[i].indexOf("=")+1);
                x=x.replace(/^\s+|\s+$/g,"");
                if (x==c_name) {
                    expireCookie(x);
                    return unescape(y);
                }
            }
        }

        function expireCookie(name) {
            var date = new Date();
            date.setTime(date.getTime()+(-1*24*60*60*1000));
            var expires = "; expires="+date.toGMTString();
            document.cookie = name+"="+expires+"; path=/; host=.ft.com; domain=.ft.com";
        }

        function addGuestPassButtonToButtonsList() {
            var html = '';
            html += '<li class="linkButton guestpass" ';
            html += ('style="z-index: 1;">');
            html += '<a class="guestPassButton" href="javascript:void(0);">';
            html += '<span>Gift Article</span></a>';
            html += '</li>';
            $('#ft-story-tools-top').prepend(html);
            $('#ft-story-tools-bottom').prepend(html);
        }

        function addGuestPassEmptyOverlay() {
            $("body").append('<div id="guestPassOverlay" title="Gift Article" class="guestPassOverlay" style="display:none;">');

            $("#guestPassOverlay").dialog({
                width: 656,
                modal: true,
                autoOpen: false,
                draggable: false,
                resizable: false
            });
        }

        function fillGuestPassOverlay(html) {
            $("#guestPassOverlay").html(html);
        }

        function openGuestPassOverlay() {
            $('#guestPassOverlay').dialog('open');
        }

        function closeGuestPassOverlay() {
            $('#guestPassOverlay').dialog('close');
        }

        function initializeForm() {
            if ($('#gp-sorry-message').length) { return; }
            populateSenderName();
            populateArticleTitle();
            revealAdditionalEmailInputs();
            validateAndHandleSubmit();
        }

        function populateArticleTitle() {
            var articleTitle = $('h1')[0].textContent || $('h1')[0].innerText;
            $('#gp-message')[0].value = $('#gp-message')[0].value.replace('#ARTICLE_TITLE#', articleTitle);
        }

        function populateSenderName() {
            var ftUserCookie = getCookie('FT_User'),
                cookieMap = {};
            if (!ftUserCookie) { return; }

            $.each(ftUserCookie.split(':'), function(index, cookieAttr) {
                var parts = cookieAttr.split('=', 2),
                    name = decodeURIComponent(parts[0]);
                cookieMap[name] = parts.length > 1 ? decodeURIComponent(parts[1]) : null;
            });

            var fullName = (cookieMap["FNAME"] || "") + " " + (cookieMap["LNAME"] || "");
            $("#gp-senderName").val($.trim(fullName));
        }

        function revealAdditionalEmailInputs() {
            $(".gp-add").click(function(){
                $("li.show").removeClass("show").next().show().addClass("show");
                if ($("li").last().hasClass("show")) {
                    $(".gp-add").addClass("disable");
                }
            });
        }

        function validateAndHandleSubmit() {
            // validator
            var gpVal = {
                fullName: function fullName() {
                    var input = $('#gp-senderName'),
                        info = input.siblings(".info");

                    if (input.val().length < 1) {
                        gpVal.errors = true;
                        if (info.length < 1) {
                            input.after('<div class="error info">Please enter your name</div>');
                            input.addClass('wrong');
                        }
                    } else {
                        info.remove();
                        input.removeClass('wrong');
                    }
                },

                email: function email() {
                    var input = $(this),
                        info = input.siblings(".info"),
                        patt = /^.+@.+[.].{2,}$/i;

                    if (((input.attr("id") == "gp-emailAddress1") || input.val().length > 0) && !patt.test(input.val())) {
                        gpVal.errors = true;

                        if (info.length < 1) {
                            input.after('<div class="error info">Please enter a valid email address</div>');
                            input.addClass('wrong');
                        }
                    } else {
                        info.remove();
                        input.removeClass('wrong');
                    }
                },

                allEmails: function allEMails() {
                    $("input[name='emailAddress']").each(gpVal.email);
                }
            };

            $('#gp-senderName').change(gpVal.fullName);
            $("input[name='emailAddress']").change(gpVal.email);

            // form buttons actions
            $('button.sendButton').click(function() {
                gpVal.errors = false;
                gpVal.fullName();
                gpVal.allEmails();
                if (!gpVal.errors) {
                    generateAndSendGuestPass($("input[name='emailAddress']"), $('#gp-message')[0].value, $('#gp-senderName')[0].value);
                }
            });

            $('button.cancelButton').click(function() {
                closeGuestPassOverlay();
            });
        }

        function generateAndSendGuestPass(emailAddresses, message, senderName) {
            var emailAddressesQS = $.map( emailAddresses, function(emailAddress) {
                return emailAddress.value !== "" ? "emailAddress=" + emailAddress.value : null;
            }).join("&");

            if (!disableSendButton) {
                disableSendButton = true;
                $.ajax({
                    url: config.guestPassBaseUrl + '/generate?' + emailAddressesQS,
                    data: {
                        articleURL: window.location.href,
                        senderName: senderName,
                        message: message
                    },
                    type: "GET",
                    dataType: "jsonp",
                    jsonpCallback: "sendGuestPassCallback",
                    cache: false,
                    success: function(data) {
                        fillGuestPassOverlay(data.html);
                        openGuestPassOverlay();
                        trackGuestPassGenerated($('#guestPassIds').val());
                    }
                });
            }
        }

        function trackGuestPassClicked() {
            FT.analytics && FT.analytics.siteIntelligence && FT.analytics.siteIntelligence.trackLinkToUntagged('guestpass_open');
        }

        function trackGuestPassGenerated(guestPassIds) {
            FT.analytics && FT.analytics.siteIntelligence && FT.analytics.siteIntelligence.sendAdditionalTracer('/guestpass', 'passes=' + guestPassIds);
        }

    return guestpass;
}());

$(function() {
    FT.guestpass.trackGuestPassRedeemed();

    $.ajax({
        url: "https://reg.ft-static.com/guestpass/assets/perm/js/jquery-ui-1.10.3.custom.min.js",
        async: false,
        type: 'GET',
        dataType: 'script',
        cache: true
    }).done(function() {
            FT.guestpass.loadGuestPassWidget();
        });
});
