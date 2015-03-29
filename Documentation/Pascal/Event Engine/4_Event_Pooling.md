<!--- This document is written in a "Markdown" language, and is best viewed on https://github.com/LaKraven/LKSL. -->
> This article is presently unfinished. A more "detailed" demo would be better.

# [Event Engine](./0_Contents.md)
## Event Pooling

## Contents:
* [What is Event Pooling?](#what-is-event-pooling)
  * [Important Note](#important-note-threads-in-a-pool-must-be-strictly-transactional)
* [Defining an Event Pool](#defining-an-event-pool)

## What is _Event Pooling?_
_Event Pooling_ takes an ordinary `TLKEventThread` descendant Type, and dynamically manages multiple instances of it. _Events_ are then passed to whichever instance of the nominated `TLKEventThread` descendant Type is most ready to process it.

The determination of which _Thread_ is best suited to process each given _Event_ is based on the average performance of each _Thread_ in the pool, as well as how many _Events_ are already waiting each each _Thread's_ respective _Queue_ and _Stack_.

_Event Pools_ are intended to be used when a singular process would genuinely benefit from being multi-threaded.

### Important Note: Threads in a Pool must be strictly Transactional!
It is important when defining a `TLKEventThread` descendant to serve as a _worker thread_ within a _Pool_ that you remember there's no way of knowing which _Thread_ is going to be handed each _Event_ to process.

Likewise, each _Thread_ within the _Pool_ has no awareness of the other _Threads_, or even how many other _Threads_ there are at any given moment.

This means that you _must_ design your `TLKEventThread` descendant (if you intend to use it within a _Pool_) as a _Transactional Thread, with no persisted state_. 

## Defining an _Event Pool_
Defining an _Event Pool_ is remarkably straight-forward.

Assuming that our _Event Thread_ type is called `TMyEventThread`, we can define an _Event Pool_ for that type with a single line of code:

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

Destroying an `Event Pool` is as simple as destroying any other object instance:

```pascal
  MyEventPool.Free;
```

The destructor will take care of finalizing the `Pool` and all other aspects of itself for you.

## Donations
Donations (while by no means mandatory) are always appreciated, and can be made by clicking this button: <a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=84FXYZX27EUJL"><img src="https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif" alt="[paypal]" /></a>
<!--- If you're reading in a plain-text editor, please copy and paste the Hyperlink into your Browser -->