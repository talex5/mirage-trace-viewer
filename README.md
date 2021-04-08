Viewer for CTF traces collected by [mirage-profile][].

To view a trace in a window using the GTK interface:

```bash
mirage-trace-viewer-gtk ./examples/net.ctf
```

To generate a JavaScript viewer in the directory `htdocs`:

```bash
mirage-trace-viewer-js --out=./htdocs ./examples/net.ctf
```

To dump a trace from a running Xen domain, use [mirage-trace-dump-xen][].

Examples can be found in the blog post [Visualising an Asynchronous Monad](http://roscidus.com/blog/blog/2014/10/27/visualising-an-asynchronous-monad/).

[mirage-profile]: https://github.com/mirage/mirage-profile
[mirage-trace-dump-xen]: https://github.com/talex5/mirage-trace-dump-xen
