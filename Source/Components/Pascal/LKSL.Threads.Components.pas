{
  LaKraven Studios Standard Library [LKSL]
  Copyright (c) 2014-2015, Simon J Stuart, All Rights Reserved

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
    - Simon J Stuart and its employees (including but not limited to directors,
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
  LKSL.Common.Types, LKSL.Threads.Main;

type
  { Forward Declarations }
  TLKThreadGeneric = class;
  TLKPrecisionThread = class;

  TLKThreadTickCallback = procedure (const ADelta, AStartTime: LKFloat) of object;

  {
    TLKThreadGeneric
      - A "Componentized" implementation of TLKThread
  }
  TLKThreadGeneric = class(TLKThread)
  private
    FOnTick: TLKThreadTickCallback;
  protected
    function GetInitialThreadState: TLKThreadState; override;
    procedure Tick(const ADelta, AStartTime: LKFloat); override;
  public
    property OnTick: TLKThreadTickCallback read FOnTick write FOnTick;
  end;

  {
    TLKPrecisionThread
      - Component Layer for a simple TLKThread
  }
  TLKPrecisionThread = class(TComponent)
  private
    FActive: Boolean;
    FThread: TLKThreadGeneric;
    // Getters
    function GetActive: Boolean;
    function GetNextTickTime: LKFloat;
    function GetOnTick: TLKThreadTickCallback;
    function GetTickRate: LKFloat;
    function GetTickRateAverage: LKFloat;
    function GetTickRateAverageOver: Cardinal;
    function GetTickRateDesired: LKFloat;
    function GetTickRateLimit: LKFloat;
    // Setters
    procedure SetActive(const AActive: Boolean);
    procedure SetOnTick(const AOnTick: TLKThreadTickCallback);
    procedure SetTickRateAverageOver(const AAverageOver: Cardinal);
    procedure SetTickRateDesired(const ATickRateDesired: LKFloat);
    procedure SetTickRateLimit(const ATickRateLimit: LKFloat);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read GetActive write SetActive;
    property NextTickTime: LKFloat read GetNextTickTime;
    property OnTick: TLKThreadTickCallback read GetOnTick write SetOnTick;
    property TickRate: LKFloat read GetTickRate;
    property TickRateAverage: LKFloat read GetTickRateAverage;
    property TickRateAverageOver: Cardinal read GetTickRateAverageOver write SetTickRateAverageOver;
    property TickRateDesired: LKFloat read GetTickRateDesired write SetTickRateDesired;
    property TickRateLimit: LKFloat read GetTickRateLimit write SetTickRateLimit;
  end;

implementation

{ TLKThreadGeneric }

function TLKThreadGeneric.GetInitialThreadState: TLKThreadState;
begin
  Result := tsPaused;
end;

procedure TLKThreadGeneric.Tick(const ADelta, AStartTime: LKFloat);
begin
  if Assigned(FOnTick) then
  begin
    Synchronize(procedure begin
                  FOnTick(ADelta, AStartTime);
                end);
  end;
end;

{ TLKPrecisionThread }

constructor TLKPrecisionThread.Create(AOwner: TComponent);
begin
  inherited;
  FThread := TLKThreadGeneric.Create;
  FActive := False;
end;

destructor TLKPrecisionThread.Destroy;
begin
  FThread.Free;
  inherited;
end;

function TLKPrecisionThread.GetActive: Boolean;
begin
  if (csDesigning in ComponentState) then
    Result := FActive
  else
    Result := (FThread.ThreadState = tsRunning);
end;

function TLKPrecisionThread.GetNextTickTime: LKFloat;
begin
  Result := FThread.NextTickTime;
end;

function TLKPrecisionThread.GetOnTick: TLKThreadTickCallback;
begin
  Result := FThread.OnTick;
end;

function TLKPrecisionThread.GetTickRate: LKFloat;
begin
  Result := FThread.TickRate;
end;

function TLKPrecisionThread.GetTickRateAverage: LKFloat;
begin
  Result := FThread.TickRateAverage;
end;

function TLKPrecisionThread.GetTickRateAverageOver: Cardinal;
begin
  Result := FThread.TickRateAverageOver;
end;

function TLKPrecisionThread.GetTickRateDesired: LKFloat;
begin
  Result := FThread.TickRateDesired;
end;

function TLKPrecisionThread.GetTickRateLimit: LKFloat;
begin
  Result := Fthread.TickRateLimit;
end;

procedure TLKPrecisionThread.SetActive(const AActive: Boolean);
const
  THREAD_STATE: Array[Boolean] of TLKThreadState = (tsPaused, tsRunning);
begin
  if (csDesigning in ComponentState) then
    FActive := AActive
  else
    FThread.ThreadState := THREAD_STATE[AActive];
end;

procedure TLKPrecisionThread.SetOnTick(const AOnTick: TLKThreadTickCallback);
begin
  FThread.OnTick := AOnTick;
end;

procedure TLKPrecisionThread.SetTickRateAverageOver(const AAverageOver: Cardinal);
begin
  FThread.TickRateAverageOver := AAverageOver;
end;

procedure TLKPrecisionThread.SetTickRateDesired(const ATickRateDesired: LKFloat);
begin
  FThread.TickRateDesired := ATickRateDesired;
end;

procedure TLKPrecisionThread.SetTickRateLimit(const ATickRateLimit: LKFloat);
begin
  FThread.TickRateLimit := ATickRateLimit;
end;

end.
