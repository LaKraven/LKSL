LaKraven Studios Standard Library [LKSL]
====

## This Library is still in EARLY DEVELOPMENT!
Please keep in mind that this library is still in its infancy, and while many of its features work very well, the interfaces are by no means "final" at this point.
In fact, there are plans to change many of them (particularly in the Event Engine units). This redesign effort is referred to internally as the "redux". You will see it mentioned quite often in recent commits.

## Installation
On *Windows*, don't forget to run **INSTALL.BAT** to register the necessary Environment Variables.

Environment Variables registered by **INSTALL.BAT** on *Windows*:

| Variable Name | Points to Path     |
| ------------- | ------------------ |
| LKSL_HOME     | \                  |
| LKSL_LIB      | \Source\Lib        |
| LKSL_PASCAL   | \Source\Lib\Pascal |


## Features:
|         Feature         | Description                                                                                      |
| ----------------------- | ------------------------------------------------------------------------------------------------ |
| Base Types              | Special *Common Base Types* each containing a Thread-Safe Locking Mechanism.                     |
| Event Engine            | A *very powerful* system for producing Multi-Threaded, Asynchronous and Event-Driven programs.   |
| Generics Collections    | Highly efficient, Thread-Safe Collection Types (*lists, trees etc.*)                             |
| High Precision Threads  | A special Thread Base Type designed to provide supremely High Precision Tick Rates.              |
| Math Library            | A library for Unit Conversion, special calculation and other useful mathematics routines.        |
| Stream Handling Library | Makes working with Streams *much* easier! Handles Deleting, Inserting, Reading and Writing data. |
| Streamables Engine      | A system to serialize Object Instances into Streams, and to dynamically reconstitute them, too.  |

## Support Matrix:

|         Feature         | Delphi (XE2-XE8) | C++ Builder (XE2-XE8) | FreePascal 3.x+ |
| ----------------------- | ---------------- | --------------------- | --------------- |
| Base Types              | Yes              | Soon                  | Yes             |
| Event Engine            | Yes              | Soon                  | Soon            |
| Generics Collections    | Yes              | Soon                  | Soon            |
| High Precision Threads  | Yes              | Soon                  | Yes             |
| Math Library            | Yes              | Soon                  | Yes             |
| Stream Handling Library | Yes              | Soon                  | Yes             |
| Streamables Engine      | Yes              | Soon                  | Soon            |

## Other Platforms currently in Consideration:

|         Feature         | C    | C++  | C#   | Java | PHP  | Python |
| ----------------------- | ---- | ---- | ---- | ---- | ---- | ------ |
| Base Types              | No   | Yes  | Yes  | Yes  | No   | Yes    |
| Event Engine            | Yes  | Yes  | Yes  | Yes  | No   | Yes    |
| Generics Collections    | No   | Yes  | Yes  | Yes  | No   | Yes    |
| High Precision Threads  | Yes  | Yes  | Yes  | Yes  | No   | Yes    |
| Math Library            | Yes  | Yes  | Yes  | Yes  | Yes  | Yes    |
| Stream Handling Library | Yes  | Yes  | Yes  | Yes  | Yes  | Yes    |
| Streamables Engine      | Yes  | Yes  | Yes  | Yes  | Yes  | Yes    |
*Note: There are no promises with the above table! It depends on who is willing to support what!**

> If there's another platform that you think could benefit from the features of the LKSL, please raise an issue. Better yet, if you feel you can produce a viable LKSL translation (in part or whole) for another programming/scripting language, please consider contributing that work back to this original repository!

## Documentation
Feel free to take a look at the [LKSL Wiki](https://github.com/LaKraven/LKSL/wiki) to learn how the LKSL can be integrated into your systems (or, as the case may be, how your system can be designed to best utilize the features of the LKSL).

You can also find educational articles demonstrating the various features of the LKSL can be found at http://otapi.com.

## Donations
Donations (while by no means mandatory) are always appreciated, and can be made by clicking this button: <a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=84FXYZX27EUJL"><img src="https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif" alt="[paypal]" /></a>
