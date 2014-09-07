unit LKSL.Demos.HPTTestThread;

interface

uses
  LKSL.Threads.Base;

type
  { Forward Declarations }
  TTestThread = class;

  { Callback Types }
  TTestThreadTickCallback = procedure(const ADelta, AStartTime, ATickRate, ATickRateAverage, ATickRateAverageOver, ATickRateLimit, ANextTick: Double) of object;

  TTestThread = class(TLKThread)
  private
    FOnTick: TTestThreadTickCallback;
    function GetOnTick: TTestThreadTickCallback;
    procedure SetOnTick(const AOnTick: TTestThreadTickCallback);
  protected
    function GetDefaultTickRateLimit: Double; override;
    procedure Tick(const ADelta, AStartTime: Double); override;
  public
    property OnTick: TTestThreadTickCallback read GetOnTick write SetOnTick;
  end;

var
  TestThread: TTestThread;

implementation

{ TTestThread }

function TTestThread.GetDefaultTickRateLimit: Double;
begin
  Result := 10.00; // 10 ticks per second
end;

function TTestThread.GetOnTick: TTestThreadTickCallback;
begin
  Lock;
  Result := FOnTick;
  Unlock;
end;

procedure TTestThread.SetOnTick(const AOnTick: TTestThreadTickCallback);
begin
  Lock;
  FOnTick := AOnTick;
  Unlock;
end;

procedure TTestThread.Tick(const ADelta, AStartTime: Double);
begin
  inherited;
  Synchronize(procedure begin
                Lock;
                if Assigned(FOnTick) then
                  FOnTick(ADelta, AStartTime, TickRate, TickRateAverage, TickRateAverageOver, TickRateLimit, NextTickTime);
                Unlock;
              end);
end;

initialization
  TestThread := TTestThread.Create;
finalization
  TestThread.Kill;

end.
