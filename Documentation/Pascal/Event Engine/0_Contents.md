<!--- This document is written in a "Markdown" language, and is best viewed on https://github.com/LaKraven/LKSL. -->
# LaKraven Studios Standard Library [LKSL]
## [Documentation](../../0_Contents.md)/[Pascal](../0_Contents.md)/Event Engine

## Contents:
* [**Introduction**](./1_Introduction.md)
  * [What is the *Event Engine*?](./1_Introduction.md#what-is-the-event-engine)
      * [In a nutshell...](./1_Introduction.md#in-a-nutshell)
  * [What is an *Event*?](./1_Introduction.md#what-is-an-event)
      * [An *Event* is *not* a *Task*!](./1_Introduction.md#an-event-is-not-a-task)
  * [What is an *Event Listener*?](./1_Introduction.md#what-is-an-event-listener)
  * [Uncoupled Modular Design](./1_Introduction.md#uncoupled-modular-design)
  * [Simple Usage Example](./1_Introduction.md#simple-usage-example)
* [**Simple Usage Example**](./2_Simple_Usage_Example.md)
  * [What is the Event Engine?](./2_Simple_Usage_Example.md#what-is-the-event-engine)
  * [Units to Include](./2_Simple_Usage_Example.md#units-to-include)
  * [Defining a custom Event Type](./2_Simple_Usage_Example.md#defining-a-custom-event-type)
      * [Lifetime Control](./2_Simple_Usage_Example.md#important-note-lifetime-control)
  * [Defining an Event Listener](./2_Simple_Usage_Example.md#defining-an-event-listener)
  * [Defining an Event Thread](./2_Simple_Usage_Example.md#defining-an-event-thread)
      * [What is an Event Thread?](./2_Simple_Usage_Example.md#what-is-an-event-thread)
      * [On to the code!](./2_Simple_Usage_Example.md#on-to-the-code)
      * [Freeing/Finalizing an Event Thread](./2_Simple_Usage_Example.md#important-note-freeingfinalizing-an-event-thread)
  * [Preparing and Dispatching an Event](./2_Simple_Usage_Example.md#preparing-and-dispatching-an-event)
* [**Scheduling Events**](./3_Scheduling_Events.md)
* [**Event Pooling**](./4_Event_Pooling.md)
  * [What is Event Pooling?](./4_Event_Pooling.md#what-is-event-pooling)
    * [Important Note](./4_Event_Pooling.md#important-note-threads-in-a-pool-must-be-strictly-transactional)
  * [Defining an Event Pool](./4_Event_Pooling.md#defining-an-event-pool)

## Donations
Donations (while by no means mandatory) are always appreciated, and can be made by clicking this button: <a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=84FXYZX27EUJL"><img src="https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif" alt="[paypal]" /></a>
<!--- If you're reading in a plain-text editor, please copy and paste the Hyperlink into your Browser -->