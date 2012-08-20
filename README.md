ghclive
=======

Google Summer of Code 2012 project, GHCi for the web

Requirements
------------
Chrome 21.x
Firefox 14.x

Does not work with Firefox 10

Quick start installation
------------------------
`git clone https://github.com/shapr/ghclive.git && cd ghclive && cabal install && ghclive` 
then point your browser to [http://localhost:3000](http://localhost:3000)

Here's some source code to paste into the editor buffer:
```haskell
import Diagrams.Prelude
import Prelude
import Network.Web.GHCLive.Display

hilbert = iterate expand mempty where
  expand t = alignBL $ hcat [u, hrule 1, reflectX u] where
             u = vcat [t, vrule 1, rotateBy (3/4) t]

ex = pad 1.1 . centerXY . lw 0.05 $ hilbert!!5
```

then type ex in the Haskell expression buffer and hit enter!

Prototypes
----------
The prototypes subdirectory contains several quick hacks demonstrating various concepts.

* [hintdownloadexecute](ghclive/tree/master/prototypes/hintdownloadexecute) is hint's [example.hs](http://code.haskell.org/hint/devel/examples/examples.hs) modified to download and execute Demo.main from http://www.scannedinavian.com/~shae/Demo.hs .
* [scottywebexecute](ghclive/tree/master/prototypes/scottywebexecute) is the basic.hs example from [scotty](https://github.com/xich/scotty/) modified only slightly to prove to myself that I understand the code.
* [hintdownloadexecute](ghclive/tree/master/prototypes/hintdownloadexecute) is the front end from http://haskell.handcraft.com/ modified to have scotty and hint as a backend instead of calling tryhaskell.org.
* [scottyjsonclock](ghclive/tree/master/prototypes/scottyjsonclock) was a quick refresher for how AJAX works in Haskell.
* [hintpostexecute](ghclive/tree/master/prototypes/hintpostexecute) uses all the previous prototypes to give a very basic ghci in the browser with Main.hs loaded from any http URL
* [jqueryconsole](ghclive/tree/master/prototypes/jqueryconsole) extends the above prototypes to use [Chris Done](https://github.com/chrisdone/)‘s [jquery-console](https://github.com/chrisdone/jquery-console) as famously seen in [tryhaskell.org](http://tryhaskell.org/), giving a more GHCi-like result
* [jqueryraw](ghclive/tree/master/prototypes/jqueryraw) is much simpler in that it uses jquery but not jquery-console.
* [svgdemo](ghclive/tree/master/prototypes/svgdemo) demonstrates SVG being returned from diagrams
* [multimport](ghclive/tree/master/prototypes/multimport) takes any number of imports from a textbox. Each line is either the http address of a file to load, or a module name to bring in scope (Data.Char).
