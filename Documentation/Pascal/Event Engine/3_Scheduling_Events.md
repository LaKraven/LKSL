<!--- This document is written in a "Markdown" language, and is best viewed on https://github.com/LaKraven/LKSL. -->
Following on directly from the [Simple Usage Example](./2_Simple_Usage_Example.md)...

# [Event Engine](./0_Contents.md)
## Scheduling Events

### What is *Event Scheduling?*
In the simplest possible terms: *Event Scheduling* places a given *Event* into a time-delayed Dispatch Queue, so that it will be Dispatched for Processing after a specified period of time.

This is particularly useful if you *know in advance* that a prepared *Event* will not become relevant for a specific period of time, but don't want to have the preparing Thread wait until that time before preparing the *Event*.

### Dispatching an *Event* through the *Event Scheduler*
As we're following on directly from the [Simple Usage Example](./2_Simple_Usage_Example.md), let's use `TMyEvent` as our example context:

```pascal
  // Schedule for the Queue...
  TMyEvent.Create('Bar').ScheduleQueue(1.5);
  // ... or Schedule for the Stack
  TMyEvent.Create('Bar').ScheduleStack(1.5);
```

In the code sample above, the value `1.5` represents the delay *in seconds* before the Scheduler dispatches the *Event* to either the *Queue* or *Stack* respectively.

Your delay can be as long or short as you like, though it should be noted that *very short* delays (below 1ms) may either be dispatched *instantly*, or up to 1ms *late*.

### Important Note: Timing Resolution and Accuracy
The Scheduler does not necessarily guarantee that your *Event* will be dispatched at *exactly the moment the delay specifies*, as the number of *Events* waiting in the Scheduler may mean that any given *Event*'s dispatch is slightly delayed.

Similarly, the resolution of the Scheduler is restricted to that of your system. If your PC cannot handle timing resolution below 1ms, then the Scheduler's resolution will be restricted to 1ms. The typical modern computer (PC, mobile, tablet etc) can provide a timing resolution of microseconds. This is more than ample for the majority of applications, such as video games and  simulations.

The Scheduler will, however, strive to provide the highest possible accuracy.

## Donations
Donations (while by no means mandatory) are always appreciated, and can be made by clicking this button: <a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=84FXYZX27EUJL"><img src="https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif" alt="[paypal]" /></a>
<!--- If you're reading in a plain-text editor, please copy and paste the Hyperlink into your Browser -->