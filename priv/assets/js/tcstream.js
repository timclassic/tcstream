/*
 * tcstream.js -- XHR/XDR Data Streaming w/periodic reconnections
 */


function TCStreamPath(ref, owner) {
    this._ref = ref;                    // Unique numeric reference, 0 or 1
    this._xhrobj = this._create_xhr();  // Get our actual object

    /* Max header length.  This constant indirectly controls the
     * maximum payload length in SYNC and DATA frames by bounding the
     * max string length of the data length fields */
    this._max_version_len = 2;
    this._max_frametype_len = 1;
    this._max_channel_len = 20;
    this._max_length_len = 6;
    this._max_seq_len = 6;

    /* Set object state */
    this._reset_state();

    /* Event handlers, settable by caller */
    this.ontimeout = new Function();
    this.onsync = new Function();
    this.onnonce = new Function();
    this.onframe = new Function();
    this.onpathend = new Function();
    this.onerror = new Function();
    this.onfatalerror = new Function();
}

TCStreamPath.prototype = {
    /*
     * Public interface
     */

    /* Connect XHR/XDR object */
    connect: function(url, nonce, attempt, last_seq, body) {
        var streamxhr = this;

        /* Some docs specify that handlers should be specified before
         * the open() call (Mozilla) */
        this._set_xhrobj_handlers();

        this._xhrobj.open('POST', url + "/path" + this._ref, true);

        /* And other browsers require that they happen after (IE) */
        this._set_xhrobj_handlers();

        /* If we're using standard XMLHttpRequest, set the
         * Content-Type.  If we're in IE, skip it, since we can't set
         * headers with XDomainRequest anyway.  It's up to the
         * webserver to know what we're up to, anyway. */
        if ('setRequestHeader' in this._xhrobj) {
            this._xhrobj.setRequestHeader("Content-Type",
                                         "text/plain;charset=UTF-8");
        }

        var data = "" + nonce
                + " " + attempt
                + " " + last_seq
                + " " + body;
        this._xhrobj.send(data);
    },

    /* Free all resources for this Path */
    destroy: function() {
        if (this._xhrobj) {
            this.disconnect();

            /* Clean up event handlers */
            if (window.XDomainRequest) {
                delete this._xhrobj.onload;
            } else {
                delete this._xhrobj.onreadystatechange;
            }
            delete this._xhrobj.onprogress;
            delete this._xhrobj.ontimeout;
            delete this._xhrobj.onerror;

            delete this._xhrobj;
            this._xhrobj = null;
        }
    },

    /* Disconnect XHR/XDR object */
    disconnect: function() {
        this._xhrobj.abort();
    },

    /* Clean up TCStreamPath object */
    reset: function() {
        this.disconnect();

        /* Clean up event handlers */
        if (window.XDomainRequest) {
            delete this._xhrobj.onload;
        } else {
            delete this._xhrobj.onreadystatechange;
        }
        delete this._xhrobj.onprogress;
        delete this._xhrobj.ontimeout;
        delete this._xhrobj.onerror;

        /* Finally, delete the XHR/XDR object itself if we are on
         * Windows.  IE8 (and perhaps other versions) will crash after
         * a little while if we don't do this.  Memory usage stays
         * pretty level using this technique.
         *
         * Chrome prefers to reuse the same object. */
        if (window.XDomainRequest) {
            delete this._xhrobj;
        }

        /* Reset state of this object */
        this._reset_state();

        /* Set up a fresh XHR/XDR object if we are on Windows.  See
         * comment above for explanation. */
        if (window.XDomainRequest) {
            this._xhrobj = this._create_xhr();
        }
    },

    /*
     * Private methods
     */

    _set_xhrobj_handlers: function() {
        var streamxhr = this;

        /* Use onload for IE, onreadystatechange otherwise to detect
         * end of path */
        if (window.XDomainRequest) {
            /* IE */
            this._xhrobj.onload = function(event) {
                streamxhr._handle_pathend(event);
            };
        } else {
            /* Others */
            this._xhrobj.onreadystatechange = function(event) {
                if (streamxhr._xhrobj.readyState === 4) {
                    streamxhr._handle_pathend(event);
                }
            };
        }

        this._xhrobj.onprogress = function(event) {
            streamxhr._handle_progress(event);
        };
        this._xhrobj.ontimeout = function(event) {
            streamxhr._handle_timeout(event);
        };
        this._xhrobj.onerror = function(event) {
            streamxhr._handle_error();
        };

        this._xhrobj.timeout = 0;
    },

    _reset_state: function() {
        /* Current response text */
        this._rt = "";

        /* Protocol state tracking */
        this._ver = 0;
        this._path_state = 'sync_frame';
        this._last_parse = 0;
        this._last_parse_timer = undefined;

        /* Frame tracking */
        this._frame_off = 0;    // Current frame offset
        this._data_off = -1;    // Current data block offset
        this._data_len = -1;    // Current data block length
        this._seq = 0;          // Sequence number for current data frame
        this._channel = "";     // Channel name for current data frame
    },

    /* Create a compatible XHR/XDR object for our use */
    _create_xhr: function() {
        var xhr = false;

        /* Use XDomainRequest if it exists (IE8 and newer) */
        if (window.XDomainRequest) {
            xhr = new XDomainRequest();

        /* Otherwise, use XMLHttpRequest */
        } else if ('XMLHttpRequest' in window) {
            if (typeof XMLHttpRequest != 'undefined') {
                /* Try to create a new XMLHttpRequest object */
                try {
                    xhr = new XMLHttpRequest();
                } catch (e) {
                    xhr = false;
                }
            }
        }

        if (!xhr) {
            throw new Error(
                "Could not find compatible XHttpRequest/XDomainRequest object"
            );
        }
        return xhr;
    },

    /* Dispatch events to proper handlers, including reference */
    _handle_progress: function(event) {
        /* Check response codes (Nothing to do here, yet) */
        if ('status' in this._xhrobj) {
            switch (this._xhrobj.status) {
            case 0:
            case 200:
            default:
                break;
            }
        }

        /* Proceed with usual protocol handling */
        this._parse_path();
    },

    _handle_pathend: function(event) {
        /* Check response codes (Nothing to do here, yet) */
        if ('status' in this._xhrobj) {
            switch (this._xhrobj.status) {
            case 0:
            case 200:
            default:
                break;
            }
        }

        /* Sanity check what the browser is telling us */
        if (this._frame_off != this._rt.length) {
            throw new Error("_handle_pathend: Frame offset is not equal to "
                            + "responseText length");
        }

        /* We've parsed all data, so trigger the end-of-path
         * handler */
        this.onpathend(this._ref);
    },

    _handle_timeout: function(event) {
        this.ontimeout(this._ref, event)
    },
    _handle_error: function() {
        this.onerror(this._ref)
    },

    _sched_parse_path: function(ms) {
        var thisxhr = this;
        return setTimeout(function() { thisxhr._parse_path(); }, ms);
    },

    /* Not for general use: only a separate function for profiling
     * purposes */
    _save_responseText: function() {
        this._rt = this._xhrobj.responseText;
    },

    _parse_path: function() {
        /* We are only allowed to run once every 100ms so that we
         * don't destroy the browser's CPU */
        var now = (new Date()).getTime();
        var diff = now - this._last_parse;
        if (diff < 100) {
            /* 100ms haven't passed yet.  Reschedule _parse_path() if
             * it has not already been done */
            if (this._last_parse_timer === undefined) {
                this._last_parse_timer = this._sched_parse_path(100 - diff + 5);
            }
            return;
        }

        /* Cancel outstanding parse timer if set */
        if (this._last_parse_timer != undefined) {
            clearTimeout(this._last_parse_timer);
            this._last_parse_timer = undefined;
        }

        /* Save aside current response text.  This is Very Important
         * for performance on Internet Explorer.  For some reason,
         * accessing responseText via the XDR object can be very
         * expensive when done repeatedly ("responseText" even shows
         * up as a "function" in IE's profiler).
         *
         * This is the *only* place in TCStreamPath where this
         * function should be called.
         *
         * Done via a separate function to allow for easier
         * profiling. */
        this._save_responseText();

        var i = 0;
        while (this._frame_off != this._rt.length) {
            try {
                switch (this._path_state) {
                    case 'sync_frame':    this._parse_sync_frame();    break;
                    case 'sync_payload':  this._parse_sync_payload();  break;
                    case 'nonce_frame':   this._parse_nonce_frame();   break;
                    case 'data_frame':    this._parse_data_frame();    break;
                    case 'data_payload':  this._parse_data_payload();  break;
                }
            }
            catch (e) {
                switch (e) {
                case 'no_data':
                    /* Not enough data has arrived, wait for next event */
                    return;
                    break;
                default:
                    throw e;
                    break;
                }
            }

            i++;
        }

        /* Set last parse time */
        this._last_parse = (new Date()).getTime();
    },

    /* Wrap parseInt() so that it fails on any string that does not
     * look exactly like an integer */
    _parseIntSafely: function(str) {
        /* Run parseInt() per digit, as this will treat strings that
         * look like floats as NaN */
        for (var i = 0; i < str.length; i++) {
            if (isNaN(parseInt(str.charAt(i)))) {
                throw new Error("Not an integer");
            }
        }
        return parseInt(str);
    },

    /* General header parsing */
    _get_next_header: function(buf, pos, sep, max) {
        var end = buf.indexOf(sep, pos)

        /* Header sanity checking */
        if (end < 0 && (buf.length - pos) > max) {
            throw 'length_exceeded';
        } else if (end > 0 && (end - pos) > max) {
            throw 'length_exceeded';
        } else if (end < 0) {
            throw 'no_data';
        }

        return {
            string: buf.substring(pos, end),
            next:   end
        };
    },

    /* Integer header parsing */
    _get_next_int_header: function(buf, pos, sep, max) {
        var hdr = this._get_next_header(buf, pos, sep, max);
        try {
            var integer = this._parseIntSafely(hdr.string);
        } catch (e) {
            throw 'not_an_integer';
        }
        return {
            integer: integer,
            string:  hdr.string,
            next:    hdr.next
        };
    },

    /* Initial SYNC frame parsing */
    _parse_sync_frame: function() {
        /* The payload of a SYNC frame consists of 2KBytes of padding,
         * used to overcome browser limitations.  Some browsers (IE8,
         * various Chromes) will not start firing `onprogress' events
         * until an initial buffer is filled.  See the following for a
         * description of this problem in IE8:
         *
         *   http://blogs.msdn.com/b/ieinternals/archive/2010/04/06/comet-streaming-in-internet-explorer-with-xmlhttprequest-and-xdomainrequest.aspx
         *
         * The sync frame consists of a protocol version using ASCII
         * numbers, a space, an ASCII `S', a space, the data length
         * using ASCII numbers, a space, and then the padding data:
         *
         *   1 S 2048 PAD...//...ENDPAD
         *
         * There should be exactly one space between each field.
         */
        var pos = 0;

        /*
         * Find version
         */
        try {
            var hdr = this._get_next_int_header(this._rt, pos, ' ',
                                                this._max_version_len);
            var ver = hdr.integer;
        } catch (e) {
            switch (e) {
            case 'length_exceeded':
                this._xhrobj.abort();
                throw new Error("Invalid frame header while waiting on " +
                                "SYNC frame");
                break;
            case 'not_an_integer':
                this._xhrobj.abort();
                throw new Error("Invalid version number: `" + hdr.string + "'");
                break;
            default:
                throw e;
                break;
            }
        }
        if (ver != 1) {
            this._xhrobj.abort();
            throw new Error("Unsupported TCStreamSession protocol version: `"
                            + ver + "'");
        }
        pos = hdr.next + 1;

        /*
         * Find SYNC frame indicator
         */
        try {
            var hdr = this._get_next_header(this._rt, pos, ' ',
                                            this._max_frametype_len);
            var frame = hdr.string;
        }
        catch (e) {
            switch (e) {
            case 'length_exceeded':
                this._xhrobj.abort();
                throw new Error("Invalid frame header while waiting on " +
                                "SYNC frame");
                break;
            default:
                throw e;
                break;
            }
        }
        if (frame != 'S') {
            this._xhrobj.abort();
            throw new Error("Unexpected frame type: `" + frame + "' " +
                            "(expected `S' for SYNC frame)");
        }
        pos = hdr.next + 1;

        /*
         * Find length field
         */
        try {
            var hdr = this._get_next_int_header(this._rt, pos, ' ',
                                                this._max_length_len);
            var datalen = hdr.integer;
        } catch (e) {
            switch (e) {
            case 'length_exceeded':
                this._xhrobj.abort();
                throw new Error("Invalid frame header while waiting on " +
                                "SYNC frame");
                break;
            case 'not_an_integer':
                this._xhrobj.abort();
                throw new Error("Invalid data length: `" + hdr.string + "'");
                break;
            default:
                throw e;
                break;
            }
        }
        pos = hdr.next + 1;

        this._ver = ver;
        this._data_off = pos;
        this._data_len = datalen;

        this._path_state = 'sync_payload';
        this._parse_sync_payload();
    },

    _parse_sync_payload: function() {
        var data_end = this._data_off + this._data_len;
        if (data_end > this._rt.length) {
            /* Not enough data has arrived, wait for next event */
            throw 'no_data';
        }
        this._frame_off = data_end;

        /* Notify user that this path is synced */
        var thisxhr = this;
        var ref = this._ref;
        setTimeout(function() { thisxhr.onsync(ref); }, 0);

        /* Set next processing stage */
        this._path_state = 'nonce_frame';
    },

    /* Initial NONCE frame parsing */
    _parse_nonce_frame: function() {
        /* The NONCE frame consists of an ASCII `N', a space, and
         * then a nonce for the next connection attempt:
         *
         *   N 923759
         *
         * There should be exactly one space between each field.
         */
        var pos = this._frame_off;

        /*
         * Find NONCE frame indicator
         */
        try {
            var hdr = this._get_next_header(this._rt, pos, ' ',
                                            this._max_frametype_len);
            var frame = hdr.string;
        }
        catch (e) {
            switch (e) {
            case 'length_exceeded':
                this._xhrobj.abort();
                throw new Error("Invalid frame header while waiting on " +
                                "NONCE frame");
                break;
            default:
                throw e;
                break;
            }
        }
        if (frame != 'N') {
            this._xhrobj.abort();
            throw new Error("Unexpected frame type: `" + frame + "' " +
                            "(expected `N' for NONCE frame)");
        }
        pos = hdr.next + 1;

        /*
         * Get nonce
         */
        var nonce = this._rt.substr(pos, 6);

        this._frame_off = pos + 6;

        /* Submit nonce to consumer */
        var thisxhr = this;
        var ref = this._ref;
        setTimeout(function() { thisxhr.onnonce(ref, nonce); }, 0);

        /* Set next processing stage */
        this._path_state = 'data_frame';
    },

    /* Initial DATA frame parsing */
    _parse_data_frame: function() {
        /* The data frame consists of an ASCII `D', a space, the
         * sequence number, a space, the channel name, a space, the
         * data length using ASCII numbers, a space, and then the
         * payload:
         *
         *   D 1234 foo 5033 PAYLOAD...//...ENDPAYLOAD
         *
         * There should be exactly one space between each field.
         */
        var pos = this._frame_off;

        /*
         * Find DATA frame indicator
         */
        try {
            var hdr = this._get_next_header(this._rt, pos, ' ',
                                            this._max_frametype_len);
            var frame = hdr.string;
        } catch (e) {
            switch (e) {
            case 'length_exceeded':
                this._xhrobj.abort();
                throw new Error("Invalid frame header while waiting on " +
                                "DATA frame");
                break;
            default:
                throw e;
                break;
            }
        }
        if (frame != 'D') {
            this._xhrobj.abort();
            throw new Error("Unexpected frame type: `" + frame + "' " +
                            "(expected `D' for DATA frame)");
        }
        pos = hdr.next + 1;

        /*
         * Find sequence number
         */
        try {
            var hdr = this._get_next_int_header(this._rt, pos, ' ',
                                                this._max_length_len);
            var seq = hdr.integer;
        } catch (e) {
            switch (e) {
            case 'length_exceeded':
                this._xhrobj.abort();
                throw new Error("Invalid frame header while waiting on " +
                                "DATA frame");
                break;
            case 'not_an_integer':
                this._xhrobj.abort();
                throw new Error("Invalid data length: `" + hdr.string + "'");
                break;
            default:
                throw e;
                break;
            }
        }
        this._seq = seq;
        pos = hdr.next + 1;

        /*
         * Find channel name
         */
        try {
            var hdr = this._get_next_header(this._rt, pos, ' ',
                                            this._max_channel_len);
            var channel = hdr.string;
        } catch (e) {
            switch (e) {
            case 'length_exceeded':
                this._xhrobj.abort();
                throw new Error("Invalid frame header while waiting on " +
                                "DATA frame");
                break;
            default:
                throw e;
                break;
            }
        }
        this._channel = channel;
        pos = hdr.next + 1;

        /*
         * Find length field
         */
        try {
            var hdr = this._get_next_int_header(this._rt, pos, ' ',
                                                this._max_length_len);
            var datalen = hdr.integer;
        } catch (e) {
            switch (e) {
            case 'length_exceeded':
                this._xhrobj.abort();
                throw new Error("Invalid frame header while waiting on " +
                                "DATA frame");
                break;
            case 'not_an_integer':
                this._xhrobj.abort();
                throw new Error("Invalid data length: `" + hdr.string + "'");
                break;
            default:
                throw e;
                break;
            }
        }
        pos = hdr.next + 1;

        this._data_off = pos;
        this._data_len = datalen;

        this._path_state = 'data_payload';
        this._parse_data_payload();
    },

    _parse_data_payload: function() {
        var data_end = this._data_off + this._data_len;
        if (data_end > this._rt.length) {
            /* Not enough data has arrived, wait for next event */
            throw 'no_data';
        }
        this._frame_off = data_end;

        var thisxhr = this;

        /* Submit data to consumer */
        var data = this._rt.substr(this._data_off, this._data_len);
        var ref = this._ref;
        var channel = this._channel;
        var seq = this._seq;
        setTimeout(function() { thisxhr.onframe(ref, seq, channel, data); }, 0);

        /* Set next processing stage, and trigger in case data is
         * already here */
        this._path_state = 'data_frame';
    }
}


function TCStreamSession(url) {
    /* XHR/XDR connections */
    this._path = new Array(2);
    this._url = url;
    this.body = "";

    /* Event handlers, settable by caller */
    this.onwarning = new Function();
    this.onwarningcleared = new Function();
    this.onerror = new Function();

    /* Mappings between channels and handlers */
    this._channels = {};

    /* Request headers */
    this._headers = {};

    /* Stream state */
    this.state = 'stopped';
    this.recovery_interval = 2000;
    this._attempt = 0;
    this._recovery_timer = undefined;
    this._last_seq = 0;
    this._next_nonce = "000000";
    this._need_nonce = false;
    this._need_reconnect = false;

    /* Warning state */
    this.warning_time = 5000;
    this._warning_state = 'cleared';
    this._warning_timer = undefined;

    /* Error handling */
    this.error_time = 30000;
    this._error_timer = undefined;

    /* Inactivity handling */
    this.inact_time = 8000;
    this._inact_timer = undefined;
}

TCStreamSession.prototype = {
    /*
     * Public interface
     */

    /* Map and unmap channel data handlers */
    map: function(channel, func) {
        this._channels[channel] = func;
    },
    unmap: function(channel) {
        delete this._channels[channel];
    },

    /* Start up stream */
    connect: function() {
        this.state = 'recovery';

        /* Schedule warning and error handlers for initial connections */
        this._set_warning_timer();
        this._set_error_timer();

        /* Trigger connection recovery code for initial connections */
        this._sched_recovery(0);
    },

    /* Disconnect stream */
    disconnect: function() {
        if (this.state === 'active' || this.state === 'recovery') {
            this.state = 'stopped';
            this._terminate_stream();
        }
    },

    /* Acknowledge a message */
    ack: function(seq) {
        this._last_seq = seq;
    },


    /*
     * Private methods
     */

    _reconnect_path: function(ref) {
        var stream = this;

        if (this._path[ref] != undefined) {
            this._path[ref].reset();
        } else {
            this._path[ref] = new TCStreamPath(ref, this);
        }

        /* Wire up event handlers */
        this._path[ref].ontimeout = function(ref) {
            stream._handle_timeout(ref);
        };
        this._path[ref].onerror = function(ref) {
            stream._handle_error(ref);
        };
        this._path[ref].onfatalerror = function(ref) {
            stream._handle_fatal_error(ref);
        };
        this._path[ref].onpathend = function(ref) {
            stream._handle_path_end(ref);
        };
        this._path[ref].onsync = function(ref) {
            stream._handle_sync(ref);
        };
        this._path[ref].onnonce = function(ref, nonce) {
            stream._handle_nonce(ref, nonce);
        };
        this._path[ref].onframe = function(ref, seq, channel, data) {
            stream._handle_frame(ref, seq, channel, data);
        };

        this._need_nonce = true;
        this._path[ref].connect(this._url, this._next_nonce, this._attempt,
                                this._last_seq, this.body);
    },

    /* Handle request timeout */
    _handle_timeout: function(ref) {
        /* Not implemented, never fired during testing.  I expect it
         * only fires if the timeout property on the XHR/XDR object is
         * nonzero. */
    },

    /* Handle request error */
    _handle_error: function(ref) {
        if (this.state === 'active') {
            this._set_warning_timer();
            this._set_error_timer();

            /* Schedule immediate recovery attempt on transtion
             * between active and recovery states */
            this.state = 'recovery';
            if (this._recovery_timer === undefined) {
                this._recovery_timer = this._sched_recovery(0);
            }
        } else {
            /* We're already in recovery mode, schedule a recovery
             * attempt if not already outstanding */
            if (this._recovery_timer === undefined) {
                this._recovery_timer =
                    this._sched_recovery(this.recovery_interval);
            }
        }
    },

    /* Handle request error */
    _handle_fatal_error: function(ref) {
        this._error();
    },

    /* Handle end of individual path */
    _handle_path_end: function(ref) {
        if (this.state === 'active' && this._need_nonce === false) {
            this._need_reconnect = false;
            this._reconnect_path(ref);
        } else {
            this._need_reconnect = true;
        }
    },

    /* Handle synced path */
    _handle_sync: function(ref) {
        /* Cancel any pending recovery, warning, and error timers, as
         * we may have just recovered from a failure state */
        if (this._recovery_timer != undefined) {
            clearTimeout(this._recovery_timer);
            this._recovery_timer = undefined;
        }
        this._cancel_warning_timer();
        this._cancel_error_timer();

        /* Reset the inactivity timer */
        this._cancel_inact_timer();
        this._set_inact_timer();

        /* If sequence number is at zero (because this is a brand new
         * session), set it to 1 */
        if (this._last_seq === 0) {
            this._last_seq = 1;
        }

        /* Finally, set session state to `active' */
        this.state = 'active';
    },

    /* Handle new nonce */
    _handle_nonce: function(ref, nonce) {
        this._next_nonce = nonce;

        /* We are no longer waiting on a nonce */
        this._need_nonce = false;

        if (this._need_reconnect === true) {
            /* If we were waiting on a nonce to reconnect the other
             * path, do so now */
            this._need_reconnect = false;
            if (ref === 0) {
                this._reconnect_path(1);
            } else {
                this._reconnect_path(0);
            }
        }
    },

    /* Handle new frame */
    _handle_frame: function(ref, seq, channel, data) {
        /* Reset the inactivity timer */
        this._cancel_inact_timer();
        this._set_inact_timer();

        /* Route frame to proper handler */
        if (channel in this._channels) {
            this._channels[channel](seq, data)
        } else {
            /* Unmapped channel, ignore data */
        }
    },

    /* Cancel warning timer if set, and notify owner that warning has
     * been cleared */
    _cancel_warning_timer: function() {
        if (this._warning_timer != undefined) {
            clearTimeout(this._warning_timer);
            this._warning_timer = undefined;

            /* If warning was raised to owner, call warning cleared
             * handler */
            if (this._warning_state === 'active') {
                this._warning_state = 'cleared';

                /* Call owner's warning cleared handler */
                var stream = this;
                setTimeout(function() { stream.onwarningcleared(); }, 0);
            }
        }
    },

    /* Set warning timer if not already set; otherwise, do nothing */
    _set_warning_timer: function() {
        if (this._warning_timer === undefined) {
            this._warning_timer = this._sched_warning(this.warning_time);
        }
    },

    _cancel_error_timer: function() {
        if (this._error_timer != undefined) {
            clearTimeout(this._error_timer);
            this._error_timer = undefined;
        }
    },

    /* Set error timer if not already set; otherwise, do nothing */
    _set_error_timer: function() {
        if (this._error_timer === undefined) {
            this._error_timer = this._sched_error(this.error_time);
        }
    },

    _sched_warning: function(ms) {
        var stream = this;
        return setTimeout(function() { stream._warning(); }, ms);
    },

    _warning: function() {
        this._warning_state = 'active';

        /* Schedule owner's warning state handler */
        var stream = this;
        setTimeout(function() { stream.onwarning(); }, 0);
    },

    _sched_error: function(ms) {
        var stream = this;
        return setTimeout(function() { stream._error(); }, ms);
    },

    _error: function() {
        /* Make sure we only run once */
        if (this.state === 'failed') {
            return;
        }
        this.state = 'failed';

        this._terminate_stream();

        /* Schedule owner's error state handler */
        var stream = this;
        setTimeout(function() { stream.onerror(); }, 0);
    },

    _terminate_stream: function() {
        /* Shut down all connections */
        for (var i = 0; i < 2; i++) {
            if (this._path[i] != undefined) {
                this._path[i].disconnect();
            }
        }

        /* Cancel all outstanding timers (do this directly to avoid
         * special handling in _cancel_*_timer functions) */
        if (this._recovery_timer != undefined) {
            clearTimeout(this._recovery_timer);
            this._recovery_timer = undefined;
        }
        if (this._warning_timer != undefined) {
            clearTimeout(this._warning_timer);
            this._warning_timer = undefined;
        }
        if (this._error_timer != undefined) {
            clearTimeout(this._error_timer);
            this._error_timer = undefined;
        }
        if (this._inact_timer != undefined) {
            clearTimeout(this._inact_timer);
            this._inact_timer = undefined;
        }
    },

    _cancel_inact_timer: function() {
        if (this._inact_timer != undefined) {
            clearTimeout(this._inact_timer);
            this._inact_timer = undefined;
        }
    },

    /* Set inact timer if not already set; otherwise, do nothing */
    _set_inact_timer: function() {
        if (this._inact_timer === undefined) {
            this._inact_timer = this._sched_inact(this.inact_time);
        }
    },

    _sched_inact: function(ms) {
        var stream = this;
        return setTimeout(function() { stream._inact(); }, ms);
    },

    _inact: function() {
        /* Trigger usual error handling */
        this._handle_error();
    },

    _sched_recovery: function(ms) {
        var stream = this;
        return setTimeout(function() { stream._recover(); }, ms);
    },

    /* Connection failure recovery.  This function should only ever be
     * called by way of _sched_recover() */
    _recover: function() {
        this._recovery_timer = undefined;

        for (var i = 0; i < 2; i++) {
            if (this._path[i] != undefined) {
                this._path[i].disconnect();
            }
        }

        /* Reset the nonce to indicate we're in recovery mode */
        this._next_nonce = "000000";

        /* Update attempt counter */
        this._attempt++;

        this._reconnect_path(0);
        this._need_reconnect = true;
        this._need_nonce = true;

        this._recovery_timer = this._sched_recovery(this.recovery_interval);
    },

    /* Free all resources for this Stream Session, including its Paths */
    destroy: function() {
        this._terminate_stream();

        for (var i = 0; i < 2; i++) {
            if (this._path[i] != undefined) {
                this._path[i].destroy();
            }
        }

        this._path = null;
    }
}
