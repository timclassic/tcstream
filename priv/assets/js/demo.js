var i = 0;
var last_num = -1;

jQuery(function() {
    var URL = window.location.protocol + "//" +
              window.location.host + "/" + tcstream_variant;
    var stream = new TCStreamSession(URL);


    /* Shut down stream gracefully upon reload.  We remove the handler
     * before page unload so that we don't change caching behavior.
     * See the following URL for some details on this approach:
     * 
     *   https://developer.mozilla.org/en-US/docs/Using_Firefox_1.5_caching#pagehide_event
     */
    var portableAddListener = function(event, handler) {
        if (window.addEventListener) {
            window.addEventListener(event, handler, false);
        } else {
            window.attachEvent("on" + event, handler);
        }
    }
    var portableRemoveListener = function(event, handler) {
        if (window.removeEventListener) {
            window.removeEventListener(event, handler, false);
        } else {
            window.detachEvent("on" + event, handler);
        }
    }
    var beforeunload_handler = function() {
        stream.disconnect();
        portableRemoveListener("beforeunload", beforeunload_handler);
    }
    var pageshow_handler = function() {
        /* Add beforeunload handler in case of a cached page load */
        portableAddListener("beforeunload", beforeunload_handler);
    }
    portableAddListener("beforeunload", beforeunload_handler);
    portableAddListener("pageshow", pageshow_handler);


    stream.body = "body_" + Math.floor(Math.random() * 0x1000000);

    stream.map('foo', function(seq, data) {
        i += data.length;
        if (i > 104857600) {
            i = 0;
            $("#streamit").append(" 100MB");
        }
        //$("#streamit").append("<br />" + "Channel foo: " + data);
        //$("#streamit").append("<br />f: " + data.length);

        stream.ack(seq);
    });
    stream.map('bar', function(seq, data) {
        //$("#streamit").append("<br />" + "Channel bar: " + data);
        //$("#streamit").append("b");

        stream.ack(seq);
    });
    stream.map('counter', function(seq, data) {
        i += data.length;

        var spc = data.indexOf(' ', 0);
        var counter = parseInt(data.substring(0, spc));

        if (counter != last_num + 1) {
            $("#streamit").append("Break in message sequence - counter: "
                                  + counter + " last_num: " + last_num +
                                 "<br />");
        }
        last_num = counter;

        //$("#streamit").append(" " + counter);
        if (i > 104857600) {
            i = 0;
            $("#streamit").append(" 100MB");
        }

        stream.ack(seq);
    });
    stream.map('reallybig_md5', function(seq, data) {
        $("#streamit").append("<br />Server's MD5 Sum is " + data);
        stream.ack(seq);
    });
    stream.map('reallybig', function(seq, data) {
        $("#streamit").append("<br />Received full message of length " + data.length);
        $("#streamit").append("<br />Client's MD5 Sum is " + btoa(md5(data, null, true)));
        stream.ack(seq);
    });

    stream.onwarning = function() {
        $("#streamit").append("<br />Entered WARNING state");
    };
    stream.onwarningcleared = function() {
        $("#streamit").append("<br />Cleared WARNING state");
    };
    stream.onerror = function() {
        $("#streamit").append("<br />Entered ERROR state, giving up");
    };

    stream.connect();
});
