var i = 0;
var last_num = -1;

jQuery(function() {
    var URL = window.location.protocol + "//" +
              window.location.host + "/" + tcstream_variant;
    var stream = new TCStreamSession(URL);

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
