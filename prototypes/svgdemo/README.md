svg demo
================================
commandResult in jquery.console.js checks first for a string result, then an array result and the final fallthrough assumes it's either a [DOM node or a jQuery object](https://github.com/chrisdone/jquery-console/blob/master/jquery.console.js#L485)

This prototype generates SVG with [diagrams-svg](https://github.com/deepakjois/diagrams-svg/) and returns the result to jquery to demonstrate displaying a DOM node.
