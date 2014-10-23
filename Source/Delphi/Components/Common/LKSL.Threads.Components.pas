{
  LaKraven Studios Standard Library [LKSL]
  Copyright (c) 2014, LaKraven Studios Ltd, All Rights Reserved

  Original Source Location: https://github.com/LaKraven/LKSL

  License:
    - You may use this library as you see fit, including use within commercial applications.
    - You may modify this library to suit your needs, without the requirement of distributing
      modified versions.
    - You may redistribute this library (in part or whole) individually, or as part of any
      other works.
    - You must NOT charge a fee for the distribution of this library (compiled or in its
      source form). It MUST be distributed freely.
    - This license and the surrounding comment block MUST remain in place on all copies and
      modified versions of this source code.
    - Modified versions of this source MUST be clearly marked, including the name of the
      person(s) and/or organization(s) responsible for the changes, and a SEPARATE "changelog"
      detailing all additions/deletions/modifications made.

  Disclaimer:
    - Your use of this source constitutes your understanding and acceptance of this
      disclaimer.
    - LaKraven Studios Ltd and its employees (including but not limited to directors,
      programmers and clerical staff) cannot be held liable for your use of this source
      code. This includes any losses and/or damages resulting from your use of this source
      code, be they physical, financial, or psychological.
    - There is no warranty or guarantee (implicit or otherwise) provided with this source
      code. It is provided on an "AS-IS" basis.

  Donations:
    - While not mandatory, contributions are always appreciated. They help keep the coffee
      flowing during the long hours invested in this and all other Open Source projects we
      produce.
    - Donations can be made via PayPal to PayPal [at] LaKraven (dot) Com
                                          ^  Garbled to prevent spam!  ^
}
unit LKSL.Threads.Components;

{
  About this unit:
    - This unit provides a component layers for the LKSL High Precision Thread types

  Changelog (latest changes first):
    23rd September 2014:
      - Prepared for Release.
}

interface

uses
  System.Classes, System.SysUtils,
  LKSL.Threads.Base;

type
  { Forward Declarations }
  TLKThreadGeneric = class;
  TLKSLPrecisionThread = class;

  TLKThreadTickCallback = procedure (const ADelta, AStartTime: Double) of object;

  {
    TLKThreadGeneric
      - A "Componentized" implementation of TLKThread
  }
  TLKThreadGeneric = class(TLKThread)
  private
    FOnTick: TLKThreadTickCallback;
  protected
    function GetInitialThreadState: TLKThreadState; override;
    procedure Tick(const ADelta, AStartTime: Double); override;
  public
    property OnTick: TLKThreadTickCallback read FOnTick write FOnTick;
  end;

  {
    TLKSLPrecisionThread
      - Component Layer for a simple TLKThread
  }
  TLKSLPrecisionThread = class(TComponent)
  private
    FActive: Boolean;
    FThread: TLKThreadGeneric;
    // Getters
    function GetActive: Boolean;
    function GetNextTickTime: Double;
    function GetOnTick: TLKThreadTickCallback;
    function GetTickRate: Double;
    function GetTickRateAverage: Double;
    function GetTickRateAverageOver: Double;
    function GetTickRateDesired: Double;
    function GetTickRateExtraTicks: Double;
    function GetTickRateExtraTime: Double;
    function GetTickRateExtraTicksAverage: Double;
    function GetTickRateExtraTimeAverage: Double;
    function GetTickRateLimit: Double;
    // Setters
    procedure SetActive(const AActive: Boolean);
    procedure SetOnTick(const AOnTick: TLKThreadTickCallback);
    procedure SetTickRateAverageOver(const AAverageOver: Double);
    procedure SetTickRateDesired(const ATickRateDesired: Double);
    procedure SetTickRateLimit(const ATickRateLimit: Double);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read GetActive write SetActive;
    property NextTickTime: Double read GetNextTickTime;
    property OnTick: TLKThreadTickCallback read GetOnTick write SetOnTick;
    property TickRate: Double read GetTickRate;
    property TickRateAverage: Double read GetTickRateAverage;
    property TickRateAverageOver: Double read GetTickRateAverageOver write SetTickRateAverageOver;
    property TickRateDesired: Double read GetTickRateDesired write SetTickRateDesired;
    property TickRateExtraTicks: Double read GetTickRateExtraTicks;
    property TickRateExtraTime: Double read GetTickRateExtraTime;
    property TickRateExtraTicksAverage: Double read GetTickRateExtraTicksAverage;
    property TickRateExtraTimeAverage: Double read GetTickRateExtraTimeAverage;
    property TickRateLimit: Double read GetTickRateLimit write SetTickRateLimit;
  end;

implementation

{ TLKThreadGeneric }

function TLKThreadGeneric.GetInitialThreadState: TLKThreadState;
begin
  Result := tsPaused;
end;

procedure TLKThreadGeneric.Tick(const ADelta, AStartTime: Double);
begin
  if Assigned(FOnTick) then
  begin
    Synchronize(procedure begin
                  FOnTick(ADelta, AStartTime);
                end);
  end;
end;

{ TLKSLPrecisionThread }

constructor TLKSLPrecisionThread.Create(AOwner: TComponent);
begin
  inherited;
  FThread := TLKThreadGeneric.Create;
  FActive := False;
end;

destructor TLKSLPrecisionThread.Destroy;
begin
  FThread.Kill;
  inherited;
end;

function TLKSLPrecisionThread.GetActive: Boolean;
begin
  if (csDesigning in ComponentState) then
    Result := FActive
  else
    Result := (FThread.ThreadState = tsRunning);
end;

function TLKSLPrecisionThread.GetNextTickTime: Double;
begin
  Result := FThread.NextTickTime;
end;

function TLKSLPrecisionThread.GetOnTick: TLKThreadTickCallback;
begin
  Result := FThread.OnTick;
end;

function TLKSLPrecisionThread.GetTickRate: Double;
begin
  Result := FThread.TickRate;
end;

function TLKSLPrecisionThread.GetTickRateAverage: Double;
begin
  Result := FThread.TickRateAverage;
end;

function TLKSLPrecisionThread.GetTickRateAverageOver: Double;
begin
  Result := FThread.TickRateAverageOver;
end;

function TLKSLPrecisionThread.GetTickRateDesired: Double;
begin
  Result := FThread.TickRateDesired;
end;

function TLKSLPrecisionThread.GetTickRateExtraTicksAverage: Double;
begin
  Result := FThread.TickRateExtraTicksAverage;
end;

function TLKSLPrecisionThread.GetTickRateExtraTicks: Double;
begin
  Result := FThread.TickRateExtraTicks;
end;

function TLKSLPrecisionThread.GetTickRateExtraTime: Double;
begin
  Result := FThread.TickRateExtraTime;
end;

function TLKSLPrecisionThread.GetTickRateExtraTimeAverage: Double;
begin
  Result := FThread.TickRateExtraTimeAverage;
end;

function TLKSLPrecisionThread.GetTickRateLimit: Double;
begin
  Result := Fthread.TickRateLimit;
end;

procedure TLKSLPrecisionThread.SetActive(const AActive: Boolean);
const
  THREAD_STATE: Array[Boolean] of TLKThreadState = (tsPaused, tsRunning);
begin
  if (csDesigning in ComponentState) then
    FActive := AActive
  else
    FThread.ThreadState := THREAD_STATE[AActive];
end;

procedure TLKSLPrecisionThread.SetOnTick(const AOnTick: TLKThreadTickCallback);
begin
  FThread.OnTick := AOnTick;
end;

procedure TLKSLPrecisionThread.SetTickRateAverageOver(const AAverageOver: Double);
begin
  FThread.TickRateAverageOver := AAverageOver;
end;

procedure TLKSLPrecisionThread.SetTickRateDesired(const ATickRateDesired: Double);
begin
  FThread.TickRateDesired := ATickRateDesired;
end;

procedure TLKSLPrecisionThread.SetTickRateLimit(const ATickRateLimit: Double);
begin
  FThread.TickRateLimit := ATickRateLimit;
end;

end.
