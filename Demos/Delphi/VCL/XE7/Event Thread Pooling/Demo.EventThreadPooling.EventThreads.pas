unit Demo.EventThreadPooling.EventThreads;

interface

uses
  System.Classes, System.SyncObjs, System.SysUtils,
  LKSL.Common.Types,
  LKSL.Threads.Main,
  LKSL.Events.Main,
  Demo.EventThreadPooling.Events;

type
  TTestEventThread = class(TLKEventThread)
  private
    FListener: TTestEventListener;
    procedure DoEvent(const AEvent: TTestEvent);
  protected
    procedure InitializeListeners; override;
    procedure FinalizeListeners; override;
  end;

var
  TestEventThread: TTestEventThread;

implementation

uses
  LKSL.Math.SIUnits,
  Demo.EventThreadPooling.MainForm;

{ TTestEventThread }

procedure TTestEventThread.DoEvent(const AEvent: TTestEvent);
var
  LDelta: LKFloat;
begin
  LDelta := GetReferenceTime - AEvent.DispatchTime;
  SYNCHRONIZE(procedure begin
                Form1.memLog.Lines.Add(Format('%s Microseconds - %s', [FormatFloat('#######################.#######################', SIMagnitudeConvert(LDelta, simOne, simMicro)), AEvent.Foo]));
              end);
end;

procedure TTestEventThread.FinalizeListeners;
begin
  inherited;
  FListener.Free;
end;

procedure TTestEventThread.InitializeListeners;
begin
  inherited;
  FListener := TTestEventListener.Create(Self, DoEvent);
end;

initialization
  TestEventThread := TTestEventThread.Create;
finalization
  TestEventThread.Kill;

end.
