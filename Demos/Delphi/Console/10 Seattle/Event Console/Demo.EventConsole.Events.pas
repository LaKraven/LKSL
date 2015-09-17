unit Demo.EventConsole.Events;

interface

uses
  LKSL.Common.Types,
  LKSL.Events.Main;

type
  TECEventTest = class(TLKEvent)

  end;

  TECEventListenerTest = class(TLKEventListener<TECEventTest>);

  TECEventConsoleHandlerTest = class(TLKEventConsoleHandler<TECEventTest>)

  end;

implementation

initialization
  TECEventConsoleHandlerTest.Register;
finalization
  TECEventconsoleHandlerTest.Unregister;

end.
