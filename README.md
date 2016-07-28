tcstream - Data Streaming over HTTP
===================================

tcstream is a system for streaming messages with low latency from an
HTTP server to a browser using only HTTP requests with chunked
replies.  The initial (and only) server implementation is built in
Erlang as a Webmachine resource.  The client is written in JavaScript
and presents a interface similar to Socket.IO.


Why not use Websockets!?
------------------------

You probably should use Websockets.  tcstream was created at a time
when Websockets were under heavy development and browser support was
spotty, at best.  In particular, IE8 was a target for the platform
that makes use of tcstream.

If you need to support really, really old browsers, or otherwise don't
want to (or cannot) use Websockets, then tcstream may be for you.


Notes
-----

Some notes:

- tcstream is unidirectional.  It is designed to efficiently send
  messages from the server to the browser.  The other direction may be
  handled via standard POSTs, PUTs, etc.


Demo
----

Coming soon!


Contributing
------------

Coming soon!


License
-------

This work is released under the Mozilla Public License 2.0.  See
[LICENSE](LICENSE) at the root of this repository.

The [tcst_file_resource module](src/tcst_file_resource.erl) was
copied from the [webmachine-demo][1] repository to serve static files
for the test harness.  Thanks Basho!

[1]: https://github.com/webmachine/webmachine-demo/blob/master/src/webmachine_demo_fs_resource.erl "Basho's demo file resource for Webmachine"
