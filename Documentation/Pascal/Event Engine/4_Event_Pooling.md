<!--- This document is written in a "Markdown" language, and is best viewed on https://github.com/LaKraven/LKSL. -->
> This article is presently unfinished. A more "detailed" demo would be better.

# [Event Engine](./0_Contents.md)
## Event Pooling

## Contents:
* [What is Event Pooling?](#what-is-event-pooling)
  * [Important Note](#important-note-threads-in-a-pool-must-be-strictly-transactional)
* [Defining an Event Pool](#defining-an-event-pool)

## What is *Event Pooling?*
*Event Pooling* takes an ordinary `TLKEventThread` descendant Type, and dynamically manages multiple instances of it. *Events* are then passed to whichever instance of the nominated `TLKEventThread` descendant Type is most ready to process it.

The determination of which *Thread* is best suited to process each given *Event* is based on the average performance of each *Thread* in the pool, as well as how many *Events* are already waiting each each *Thread's* respective *Queue* and *Stack*.

*Event Pools* are intended to be used when a singular process would genuinely benefit from being multi-threaded.

### Important Note: Threads in a Pool must be strictly Transactional!
It is important when defining a `TLKEventThread` descendant to serve as a *worker thread* within a *Pool* that you remember there's no way of knowing which *Thread* is going to be handed each *Event* to process.

Likewise, each *Thread* within the *Pool* has no awareness of the other *Threads*, or even how many other *Threads* there are at any given moment.

This means that you *must* design your `TLKEventThread` descendant (if you intend to use it within a *Pool*) as a *Transactional Thread, with no persisted state*. 

## Defining an *Event Pool*
Defining an *Event Pool* is remarkably straight-forward.

Assuming that our *Event Thread* type is called `TMyEventThread`, we can define an *Event Pool* for that type with a single line of code:

```pascal
  TMyEventPool = class(TLKEventPool<TMyEventThread>);
```

We can now create an instance of our `TMyEventPool` type like this:

```pascal
var
  MyEventPool: TMyEventPool;
begin
  MyEventPool := TMyEventPool.Create(TThread.ProcessorCount);
end;
```

The parameter value `TThread.ProcessorCount` (in Delphi) retrieves the number of CPU Threads available on the user's system. You can substitute this for any integer value greater than 1.

Destroying an *Event Pool* is as simple as destroying any other object instance:

```pascal
  MyEventPool.Free;
```

The destructor will take care of finalizing the *Pool* and all other aspects of itself for you.

## Donations
Donations (while by no means mandatory) are always appreciated, and can be made by clicking this button: <a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=84FXYZX27EUJL"><img src="https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif" alt="[paypal]" /></a>
<!--- If you're reading in a plain-text editor, please copy and paste the Hyperlink into your Browser -->