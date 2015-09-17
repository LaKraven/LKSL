unit Demo.EventConsole.Main;

interface

uses
  System.Classes, System.SysUtils,
  LKSL.Common.Types,
  LKSL.Events.Main,
  Demo.EventConsole.Events;

type
  TECEventThread = class(TLKEventThread)
  private
    FListenerTest: TECEventListenerTest;
    procedure DoEventTest(const AEvent: TECEventTest);
  protected
    procedure InitializeListeners; override;
    procedure FinalizeListeners; override;
  end;

var
  ECEventThread: TECEventThread;

implementation

{ TECEventThread }

procedure TECEventThread.DoEventTest(const AEvent: TECEventTest);
begin
  Writeln('DoEventTest fired');
end;

procedure TECEventThread.FinalizeListeners;
begin
  inherited;
  FListenerTest.Free;
end;

procedure TECEventThread.InitializeListeners;
begin
  inherited;
  FListenerTest := TECEventListenerTest.Create(Self, DoEventTest);
end;

initialization
  ECEventThread := TECEventThread.Create(ermAutomatic);
finalization
  ECEventThread.Free;

end.
