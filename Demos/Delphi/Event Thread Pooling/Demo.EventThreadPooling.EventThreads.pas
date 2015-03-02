unit Demo.EventThreadPooling.EventThreads;

interface

uses
  System.Classes, System.SyncObjs,
  LKSL.Common.Types,
  LKSL.Threads.Main,
  LKSL.Events.Base,
  Demo.EventThreadPooling.Events;

type
  TLKDemoEventThread = class(TLKEventThread)
  private
    FEventListener: TLKDemoEventListener;
    procedure DoEvent(const AEvent: TLKDemoEvent);
  protected
    procedure RegisterListeners; override;
    procedure UnregisterListeners; override;
  end;

var
  DemoEventThread: TLKDemoEventThread;

implementation

{ TLKDemoEventThread }

procedure TLKDemoEventThread.DoEvent(const AEvent: TLKDemoEvent);
begin
  // Simulate some "work"
  Sleep(Random(10));
  TLKDemoResponseEvent.Create.Queue;
end;

procedure TLKDemoEventThread.RegisterListeners;
begin
  FEventListener := TLKDemoEventListener.Create(Self, DoEvent, easAuto);
end;

procedure TLKDemoEventThread.UnregisterListeners;
begin
  FEventListener.Free;
end;

initialization
  DemoEventThread := TLKDemoEventThread.Create;
finalization
  DemoEventThread.Kill;

end.
