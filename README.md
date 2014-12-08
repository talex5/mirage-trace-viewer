Viewer for traces collected by [mirage-profile][].

There are three optional features here:

* A native GTK viewer (`./configure --enable-gtk`)
* A JavaScript HTML 5 canvas viewer (`./configure --enable-javascript`)
* A tool for collecting trace data from a Xen guest (`./configure --enable-collect`), which can then be viewed by the above tools

Run `mirage-trace-viewer --help` for instructions.

Examples can be found in the blog post [Visualising an Asynchronous Monad](http://roscidus.com/blog/blog/2014/10/27/visualising-an-asynchronous-monad/).

[mirage-profile]: https://github.com/mirage/mirage-profile
