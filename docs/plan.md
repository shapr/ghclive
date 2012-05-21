The Plan™
---------

Design documents for the "multiuser browser-based GHCi" project. Also includes information about world domination in 3.142 easy steps.


Milestone: "Local Evaluation Server"
====================================

Goal: Create a quick & dirty Haskell interpreter that runs on your local machine and displays data as HTML in the browser.

For reasons of flexibility, the interpreter is built using the [hint][] package, which in turn is built on the GHC API.

Tasks:

1. Check out the [hint][] package by building a rudimentary interpreter in the command line that can

    1.1. Load modules. Reload modules.

    1.2. Evaluate expressions.

2. Turn that interpreter into a small server, the *evaluation server*.

    2.1. Very simple HTTP-Rest API. Hm, does it really have to be HTTP?

    Note: At some point, we want to include the interpreter server in the Haskell platform, so the fewer dependencies, the better. Heinrich leans towards [happstack-lite][], but also likes the approach taken in the [msgpack-rpc][] package. (The latter may be the right abstraction later, if we deal with closures.)

    2.2. Modules are loaded from URLs. (Can be from the local file system or form the remote. Allows us to get source modules directly from hpase, etherpad, github, etc.)

3. Make a small JavaScript API for this server.

    3.1. Mainly a JS object `HsInterpreter` that supports functions like

        HsInterpreter.load("http://hpaste.org/example");
        result = HsInterpreter.evaluate("take 5 $ iterate (+1) 0");
        HsInterpreter.reload();
    
    3.2. Use this API for a small demonstration `.html` page. Note: The web page is not served by the evaluation server, it's independent. For fun: Adapt the TryHaskell webpage to make use of the JS API.

4. Replace the `Show` class.

    4.1. New class `Display` that returns HTML instead of `String`. **Poll class name?**
    
    4.2. Evaluation server wraps everything into a call to `display`.
    
    4.3. Make sure that the JavaScript API can handle HTML results. Integrate into demo page.

5. Integrate with Emacs by adding polling / notifications.

    5.1. Server can receive expressions from several clients and send the results to several clients
    
    5.2. JavaScript API can poll / be notified of new expressions and results to add to the session. 
    
    5.2. Make Emacs send expressions to the server.

A Collection of Use Cases
=========================

Here a collection of advanced features.

* Display infinite values by presenting only partial values and providing a "more" link for more. Maybe this can also be used for computations that have timed out? Péter Diviánszky has done some pretty-printing in this direction.

* Tangible values. The idea is that the value returned is interactive, for instance it's a function `Double -> Double` which is displayed with several sliders that the user can interact with. Luite Stegeman has done this without server-side state, but I don't know how general that is.

    Related idea: Input values. Display a graphical widget that will generate a value that can be used subsequently. This is like calling `getLine` in GHCi, except way more general.

* Run continuous computations on the evaluation server. For instance, Chris Smith displays animations and runs games on the server side. Heinrich wants to do audio live coding, where the server has to produce a never-ending audio stream.

* Interactive Haddock documentation. Examples in Haddock can be evaluated live. Similar to Péter Diviánszky's active-hs interactive documents.

* Display output from `stdout`, feeed input to `stdin`.

* Immediate feedback: each numeric value in an expression can pop up a slider and the expression is evaluated dynamically.

* Immediate feedback 2: Fill a whole module/worksheet inside the Haskell interpreter environment. That's how Chris Smith develops his programs.

* Live coding. Again, the interpreter session is actually a worksheet/notebook. Editing code will make live changes to the evaluation results.



  [hint]: http://hackage.haskell.org/package/hint
  [hs-json-rpc]: http://hackage.haskell.org/package/hs-json-rpc
  [happstack-lite]: http://www.happstack.com/C/ViewPage/9
  [msgpack-rpc]: http://hackage.haskell.org/package/msgpack-rpc

