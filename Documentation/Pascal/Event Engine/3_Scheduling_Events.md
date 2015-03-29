<!--- This document is written in a "Markdown" language, and is best viewed on https://github.com/LaKraven/LKSL. -->
Following on directly from the [Simple Usage Example](./2_Simple_Usage_Example.md)...

# [Event Engine](./0_Contents.md)
## Scheduling Events

### What is _Event Scheduling?_
In the simplest possible terms: _Event Scheduling_ places a given _Event_ into a time-delayed Dispatch Queue, so that it will be Dispatched for Processing after a specified period of time.

This is particularly useful if you _know in advance_ that a prepared _Event_ will not become relevant for a specific period of time, but don't want to have the preparing Thread wait until that time before preparing the _Event_.

### Dispatching an _Event_ through the _Event Scheduler_
As we're following on directly from the [Simple Usage Example](./2_Simple_Usage_Example.md), let's use `TMyEvent` as our example context:

```pascal
  // Schedule for the Queue...
  TMyEvent.Create('Bar').ScheduleQueue(1.5);
  // ... or Schedule for the Stack
  TMyEvent.Create('Bar').ScheduleStack(1.5);
```

In the code sample above, the value `1.5` represents the delay _in seconds_ before the Scheduler dispatches the _Event_ to either the _Queue_ or _Stack_ respectively.

Your delay can be as long or short as you like, though it should be noted that _very short_ delays (below 1ms) may either be dispatched _instantly_, or up to 1ms _late_.

### Important Note: Timing Resolution and Accuracy
The Scheduler does not necessarily guarantee that your _Event_ will be dispatched at _exactly the moment the delay specifies_, as the number of _Events_ waiting in the Scheduler may mean that any given _Event_'s dispatch is slightly delayed.

Similarly, the resolution of the Scheduler is restricted to that of your system. If your PC cannot handle timing resolution below 1ms, then the Scheduler's resolution will be restricted to 1ms. The typical modern computer (PC, mobile, tablet etc) can provide a timing resolution of microseconds. This is more than ample for the majority of applications, such as video games and  simulations.

The Scheduler will, however, strive to provide the highest possible accuracy.

## Donations
Donations (while by no means mandatory) are always appreciated, and can be made by clicking this button: <a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=84FXYZX27EUJL"><img src="https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif" alt="[paypal]" /></a>
<!--- If you're reading in a plain-text editor, please copy and paste the Hyperlink into your Browser -->