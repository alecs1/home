Basics:
-use the Create buttons on the upper left to populate the view
-use the Delete buttons on the lower left to delete the created objects
-if you are running Linux run the script and not executable to make sure the needed libraries are found. If you are running Konqueror don't click on the script, run in from a console to make shure the path is kept.
-press Settings/"Show/Hide Dock" to make more space for the objects
-left click on the objects area to make it fullscreen
-scroll over the objects area to increase or decrease the number of frames per second
-try the PuzzleWorldShow, it is spectacular


		Parallel Worlds, the space simulator.
		© 2007, Alex Dănilă


(Very) Small introduction.
Parallel Worlds is a simple program that can show objects movement respecting as much as posible geometry and time computation. (Most geometry computations are 3d, showing them is not).
At startup a world containing 8 bees following each other is initalised.

The central widget is the one that shows all the objects, it is named universe and can hold many worlds. A world is a set of objects that live together and which can interact. Objects from different worlds cannot interact.

On the left is the control widget, a dock with a main menu and two lists. The first list shows world types, click create to make a new instance of a world. The second list shows the world instances, and offers control over the world. For the moment it only allows destructions of a world.
The control widget can be detached in order to create more space for the universe.

The objects' SPEED IS RESPECTED no matter what the fps and the procesor load is. The movement precision can vary with fps, and if the fps is too low colisions will be aproximated very imprecise  (grosolan is the word in Romanian but I don't remember some corresponding word in English :) ).
The objects area also allows setting a frame per second target, which the program will try to achieve (on the upper left information about the fps is shown).The exact fps depends not only on the procesor load, but also on the division of your system's frequency with the fps wanted.
Large values (over 75 fps) can make the objects move inconstant (the objects will appear to jump from time to time). This is a bad combination between my code and the Qt's timers and signals.

On Linux the performance varies greatly (from shit to the impresive drawing of 1000 bee objects at 25 fps), mostly because of X11 and I don't know how to make it stay good. On SuSE it was always shit. On Windows performance is cool (on Linux Wine performance is also good :) ).

The picture has a nice story, it was created for a Missionaries and Cannibals simulation for school. It took me more than 3 hours with The Gimp to create it (stupid Gnome style interface, a crash and not understanding some of its functionalities gave me a hard time). The image contains 4 patterns from Gimp, 3 patterns I took from pictures, 1 beautiful girl and a part of an incredible picture presenting a feerical (is this the word in English?) island.

The icon is my first experience with Inkscape, quite pleasant, and much shorter.

