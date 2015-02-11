LaKraven Studios Standard Library [LKSL]
====

This library provides a vast (and growing) range of powerful features for the following programming languages:
  - Delphi
  - C++ Builder
  - FreePascal/Lazarus

This Library is still in EARLY DEVELOPMENT!
====
Please keep in mind that this library is still in its infancy, and while many of its features work very well, the interfaces are by no means "final" at this point.
In fact, there are plans to change many of them (particularly in the Event Engine units). Also, unit names will soon be changing (but that's a one-time thing, easy to resolve in your implementations).

Library Installation for Delphi
====
Please run the LKSLInstall.exe file to configure your version(s) of the IDE to use the LKSL. It simply adds the LKSL source paths to RAD Studio's respective Search Paths. This executable can be re-run each time new paths are introduced in the LKSL.

Component Installation for Delphi
====
Please load \Packages\Delphi\<version>\LKSL_Components<version>.dproj in Delphi, right-click on the Project Manager and click "Install".

> NOTE: C++ Builder versions will be added at a slower rate (unless you want to help producing verbatim C++ translations of the Delphi units, which would be very-much welcomed)

Current Features
====
  - Stream Handling Library (makes working with Streams much easier)
  - Streamables Engine (an engine for serializing and loading custom objects to/from Streams)
  - High Precision Threading (a special Thread type which provide extremely accurate "reference times" for interpolation)
  - Event Engine (a VERY powerful, highly optimized engine for producing Multi-Threaded, Asynchronous, Event-Driven systems)

Documentation
====
Comprehensive Documentation (as well as Unit Tests and Demos) are to be reintroduced soon, but in the meantime, educational articles demonstrating the various features of the LKSL can be found at http://otapi.com.

Donations
====
Donations (while by no means mandatory) are always appreciated, and can be made by clicking the button to the right: <a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=84FXYZX27EUJL"><img src="https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif" alt="[paypal]" /></a>
