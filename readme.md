ECMAchine
=========

Introduction
------------

ECMAchine is an in-browser Scheme REPL that is also a toy operating system. It has a virtual filesystem that is accessed through Unix-like commands and a rudimentary process management system.

_Lots of examples will follow soon!_

To Do
-----

- Language 
 - ; comments
 - Primitives for AJAX requests
 - Newlines should be preserved when:
     - writing to files
     - displaying function contents
- Library
 - (accumulate), more higher-order functions?
 - wrappers for the AJAX primitives
- Processes
 - Processes (and scripts) should have their own environments rather than using global environment
 - Allow passing arguments to a process/script
 - Refactor processes into a new class (like Filesystem)
 - More accurate performance measurement (rather than just adding up all evals and dividing by total time)
 - Bonus: come up with a simple time-sharing system that works in JavaScript? 
     - For example, processes could continually adjust their interval in an attempt to reach ~1000 evals/sec systemwide 
     - Could timesharing apply to scripts (one-time operations) in addition to processes (recurring operations)?
- Overlays
 - Allow overlays to be draggable?
 - Figure out a solution to the overlay-getting-in-the-way-of-text problem
- Programs
 - It would be very cool to actually get some more involved programs working: e.g.
     - Text editor
     - Web browser
 - This involves lots of challenges: e.g. how to get user input in a program