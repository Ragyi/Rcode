/**
 * Created by raibrahim on 19/05/2016.
 */

//Google Analytics
//This code needs to be implemented on your site after your Google Analytics code has had a chance to run
//If you have your GA code running in the <head> section of your site, this code should be implemented near the closing </body> tag.



//In GA Classic, we're setting a custom variable/
    function readCookie(name) {
        name += '=';
        for (var ca = document.cookie.split(/;\s*/),
                 i = ca.length - 1; i >= 0; i--)
            if (!ca[i].indexOf(name))
                return ca[i].replace(name, '');
    }

var gaUserCookie = readCookie("__utma");

if (gaUserCookie != undefined) {
    var cookieValues = gaUserCookie.split('.');

    if (cookieValues.length > 1) {
        var userId = cookieValues[1];
        try {
            _gaq.push(['_setCustomVar',1,'gaUserId',userId,1]);
            _gaq.push(['_trackEvent', 'Custom Variables', 'Set UserId','',0,true]);
        } catch(e) {}
    }
}

//Google Universal Analytics
//In GA Universal we're setting a Custom Dimension
//We will obviously need to create the custom dimension within GTM before we can store data in it

    function readCookie(name) {
        name += '=';
        for (var ca = document.cookie.split(/;\s*/),
                 i = ca.length - 1; i >= 0; i--)
            if (!ca[i].indexOf(name))
                return ca[i].replace(name, '');
    }

var gaUserCookie = readCookie("_ga");

if (gaUserCookie != undefined) {
    var cookieValues = gaUserCookie.split('.');
    if (cookieValues.length > 2 )
    {
        var userId = cookieValues[2];
        try {
            ga('set', 'dimension1', userId); //Dimensions number would be first avaliable custom dimension
            ga('send', 'event', 'Custom Variables', 'Set UserId', {'nonInteraction': 1});
        } catch(e) {}
    }
}

// Google Tag Manager with Universal Analytics
// Create a Custom HTML Tag within GTM
// Create a UserId variable in the dataLayer

    function readCookie(name) { //Cookie name
        name += '=';
        for (var ca = document.cookie.split(/;\s*/),
                 i = ca.length - 1; i >= 0; i--)
            if (!ca[i].indexOf(name))
                return ca[i].replace(name, '');
    }
var gaUserCookie = readCookie("_ga");
if (gaUserCookie != undefined)  {
    var cookieValues = gaUserCookie.split('.');
    if (cookieValues.length > 2 )  {
        var userId = cookieValues[2];
        dataLayer.push({'event':'setUserId', 'userId': userId});
    }
}
