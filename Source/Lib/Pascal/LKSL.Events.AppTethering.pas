unit LKSL.Events.AppTethering;

interface

uses
  System.Classes, System.SysUtils,
  LKSL.Common.Types,
  LKSL.Events.Base,
  IPPeerClient, IPPeerServer, System.Tether.Manager, System.Tether.AppProfile;

type
  { Forward Declarations }
  TLKEventTethering = class;

  {
    TLKEventTethering
      - Uses "App Tethering" to send and receive Events between process and/or systems
  }
  TLKEventTethering = class(TLKEventTransmitterBase)
  private
    FTetheringManager: TTetheringManager;
    FTetheringProfile: TTetheringAppProfile;

    function GetManagerPassword: String;
    function GetManagerTitle: String;

    procedure SetManagerPassword(const AValue: String);
    procedure SetManagerTitle(const AValue: String);

    procedure OnEndManagersDiscovery(const Sender: TObject; const RemoteManagers: TTetheringManagerInfoList);
    procedure OnRequestManagerPassword(const Sender: TObject; const RemoteIdentifier: string; var Password: string);
    procedure OnResourceReceived(const Sender: TObject; const AResource: TRemoteResource);
  protected
    function ReceiveFromManager(const ATetheringManagerInfo: TTetheringManagerInfo): Boolean; virtual;
    function TransmitToProfile(const ATetheringProfileInfo: TTetheringProfileInfo): Boolean; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure TransmitEvent(const AEvent: TLKEvent; const AEventStream: TMemoryStream); override;

    property ManagerPassword: String read GetManagerPassword write SetManagerPassword;
    property ManagerTitle: String read GetManagerTitle write SetManagerTitle;
  end;


implementation

uses
  LKSL.Streamables.Main;

{ TLKEventTethering }

constructor TLKEventTethering.Create;
begin
  inherited;
  FTetheringManager := TTetheringManager.Create(nil);
  FTetheringProfile := TTetheringAppProfile.Create(nil);
  FTetheringProfile.Manager := FTetheringManager;
  FTetheringManager.OnEndManagersDiscovery := OnEndManagersDiscovery;
  FTetheringProfile.OnResourceReceived := OnResourceReceived;
  FTetheringManager.OnRequestManagerPassword := OnRequestManagerPassword;
  SetManagerPassword('1234');
end;

destructor TLKEventTethering.Destroy;
begin
  FTetheringProfile.Free;
  FTetheringManager.Free;
  inherited;
end;

function TLKEventTethering.GetManagerPassword: String;
begin
  Lock;
  Result := FTetheringManager.Password;
  Unlock;
end;

function TLKEventTethering.GetManagerTitle: String;
begin
  Lock;
  Result := FTetheringManager.Text;
  Unlock;
end;

procedure TLKEventTethering.OnEndManagersDiscovery(const Sender: TObject; const RemoteManagers: TTetheringManagerInfoList);
var
  I: Integer;
begin
  for I := 0 to RemoteManagers.Count - 1 do
    if ReceiveFromManager(RemoteManagers[I]) then
      FTetheringManager.PairManager(RemoteManagers[I]);
end;

procedure TLKEventTethering.OnRequestManagerPassword(const Sender: TObject; const RemoteIdentifier: string; var Password: string);
begin
  Password := FTetheringManager.Password;
end;

procedure TLKEventTethering.OnResourceReceived(const Sender: TObject; const AResource: TRemoteResource);
var
  LEventStream: TStream;
  LEvent: TLKEvent;
begin
  // Validate as Event
  if AResource.ResType = TRemoteResourceType.Stream then
  begin
    LEventStream := AResource.Value.AsStream;

    LEvent := TLKEventType(Streamables.GetStreamableTypeFromStream(LEventStream)).Create;
    case LEvent.Priority of
      epQueue: LEvent.Queue;
      epStack: LEvent.Stack;
    end;
  end;
end;

function TLKEventTethering.ReceiveFromManager(const ATetheringManagerInfo: TTetheringManagerInfo): Boolean;
begin
  // Assume it's all good
  Result := True;
end;

procedure TLKEventTethering.SetManagerPassword(const AValue: String);
begin
  Lock;
  FTetheringManager.Password := AValue;
  Unlock;
end;

procedure TLKEventTethering.SetManagerTitle(const AValue: String);
begin
  Lock;
  FTetheringManager.Text := AValue;
  Unlock;
end;

procedure TLKEventTethering.TransmitEvent(const AEvent: TLKEvent; const AEventStream: TMemoryStream);
var
  I: Integer;
begin
  for I := 0 to FTetheringManager.PairedManagers.Count - 1 do
    FTetheringManager.UnPairManager(FTetheringManager.PairedManagers[I]);
  FTetheringManager.DiscoverManagers;
  for I := 0 to FTetheringManager.RemoteProfiles.Count - 1 do
  begin
    if TransmitToProfile(FTetheringManager.RemoteProfiles[I]) then
    begin
      FTetheringProfile.Connect(FTetheringManager.RemoteProfiles[I]);
      FTetheringProfile.SendStream(FTetheringManager.RemoteProfiles[I], 'Event Stream', AEventStream)
    end;
  end;
end;

function TLKEventTethering.TransmitToProfile(const ATetheringProfileInfo: TTetheringProfileInfo): Boolean;
begin
  Result := True; // Assume we can!
end;

end.
