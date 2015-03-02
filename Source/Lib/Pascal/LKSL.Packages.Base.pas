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
unit LKSL.Packages.Base;

interface

{
  About this unit:
    - This unit provides type declarations required for our "Package Engine"
    - This unit also provides the Abstract Base Implementation for the same.

  Included Usage Demos:
    - "LKSL_Demo_PafckageEngine_Basic" in the "\Demos\Delphi\<version>\Package Engine\Basic" folder

  Changelog (latest changes first):
    16th October 2014:
      - Prepared for Release
}

uses
  System.Classes, System.SysUtils, System.SyncObjs,
  LKSL.Common.Types,
  LKSL.Streamables.Main;

type
  { Forward Declarations }
  TLKAssetManagerBase = class;
  TLKAsset = class;
  TLKPackageManager = class;
  TLKPackage = class;

  { Array Types }
  TLKAssetManagerBaseArray = Array of TLKAssetManagerBase;
  TLKAssetArray = Array of TLKAsset;
  TLKPackageArray = Array of TLKPackage;

  {
    TLKAssetManagerBase
      - Abstract Base Class for all Asset Managers
      - An "Asset Manager" represents a particular type of file
  }
  TLKAssetManagerBase = class abstract(TLKPersistent)
  private
    FAssets: TLKAssetArray;
    FIndex: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    // You MUST override "GetManagerGUID" and provide a unique GUID String for each Asset Manager Type
    // This is internally serialized against each File/Stream within a Package.
    function GetManagerGUID: String; virtual; abstract;
  end;

  {
    TLKAsset
      - An Asset contained within a Package
      - Works on a principal of Caching from the Source Stream
      - CAUTION: WILL DUPLICATE MEMORY CONSUMPTION IF THE PACKAGE IS IN A MEMORY STREAM!
  }
  TLKAsset = class(TLKPersistent)
  private
    FIndex: Integer;
    FAssetManager: TLKAssetManagerBase;
    FBlockStart: Int64;
    FBlockSize: Int64; // Stored internally to avoid unnecessary lookups within the Package Stream
    FName: String;
    FPackage: TLKPackage;
    function GetName: String;
    procedure SetName(const AName: String);
  public
    constructor Create(const APackage: TLKPackage); reintroduce; overload;
    constructor Create(const AAssetManager: TLKAssetManagerBase); reintroduce; overload;
    destructor Destroy; override;

    property AssetManager: TLKAssetManagerBase read FAssetManager;
    property BlockStart: Int64 read FBlockStart;
    property BlockSize: Int64 read FBlockSize;
    property Name: String read GetName write SetName;
    property Package: TLKPackage read FPackage;
  end;

  {
    TLKPackageManager
      - Heart and soul of the Package Engine
      - Used to load and manage existing Packages (from Stream or File)
      - Used to produce and manage NEW Packages (which can be saved to Stream or File)
      - Asset Managers are automatically registered with the Package Manager on Construction
        susbequently removed on destruction. This instructs the Package Manager how to handle
        particular types of File/Stream within a Package
  }
  TLKPackageManager = class(TLKPersistent)
  private
    FAssetManagers: TLKAssetManagerBaseArray;
    FPackages: TLKPackageArray;
    procedure AddAssetManager(const AAssetManager: TLKAssetManagerBase);
    procedure AddPackage(const APackage: TLKPackage);
    procedure ClearAssetManagers;
    procedure ClearPackages;
    procedure DeleteAssetManager(const AAssetManager: TLKAssetManagerBase);
    procedure DeletePackage(const APackage: TLKPackage);
    function GetAssetManagerByGUID(const AGUID: String): TLKAssetManagerBase;
    function GetAssetManagerIndexByGUID(const AGUID: String): Integer;
    function GetPackageByName(const AName: String): TLKPackage;
    function GetPackageIndexByName(const AName: String): TLKPackage;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Packages[const AName: String]: TLKPackage read GetPackageByName; default;
  end;

  {
    TLKPackage
      - An actual Package
      - Can be transmitted as a Stream
      - Can be stored as a File
      - You can add and save individual objects/files into the Package
  }
  TLKPackage = class(TLKStreamableNamed)
  private
    FAssets: TLKAssetArray;
    FIndex: Integer;
    procedure AddAsset(const AAsset: TLKAsset);
    procedure ClearAssets;
    procedure DeleteAsset(const AAsset: TLKAsset);
    function GetAssetByIndex(const AIndex: Integer): TLKAsset;
    function GetAssetByName(const AName: String): TLKAsset;
    function GetAssetIndexByName(const AName: String): Integer;
  public
    constructor Create; overload; override;
    constructor Create(const APackageName: String); reintroduce; overload;
    constructor CreateFromFile(const AFileName: String);
    destructor Destroy; override;

    property Assets[const AIndex: Integer]: TLKAsset read GetAssetByIndex; default;
    property Assets[const AName: String]: TLKAsset read GetAssetByName; default;
  end;

var
  PackageManager: TLKPackageManager;

implementation

{ TLKAssetManagerBase }

constructor TLKAssetManagerBase.Create;
begin
  inherited Create;
end;

destructor TLKAssetManagerBase.Destroy;
begin

  inherited;
end;

{ TLKAsset }

constructor TLKAsset.Create(const APackage: TLKPackage);
begin
  inherited Create;
  FPackage := APackage;
  FName := FPackage.GetFirstAvailableName;
  FPackage.AddAsset(Self);
end;

constructor TLKAsset.Create(const AAssetManager: TLKAssetManagerBase);
begin
  inherited Create;
  FAssetManager := AAssetManager;
  FName := FAssetManager.GetFirstAvailableName;
  FAssetManager.AddAsset(Self);
end;

destructor TLKAsset.Destroy;
begin

  inherited;
end;

function TLKAsset.GetName: String;
begin
  Lock;
  Result := FName;
  Unlock;
end;

procedure TLKAsset.SetName(const AName: String);
begin
  Lock;
  FName := AName;
  if FAssetManager <> nil then
  begin
    FAssetManager.DeleteAsset(Self);
    FAssetManager.AddAsset(Self);
  end;
  if FPackage <> nil then
  begin
    FPackage.DeleteAsset(Self);
    FPackage.AddAsset(Self);
  end;
  Unlock;
end;

constructor TLKPackage.CreateFromFile(const AFileName: String);
begin

end;

{ TLKPackageManager }

procedure TLKPackageManager.AddAssetManager(const AAssetManager: TLKAssetManagerBase);
  function GetSortedPosition: Integer;
  var
    LIndex, LLow, LHigh: Integer;
  begin
    Result := 0;
    LLow := 0;
    LHigh := Length(FAssetManagers) - 1;
    if LHigh = - 1 then
      Exit;
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if AAssetManager.GetManagerGUID <= FAssetManagers[LIndex].GetManagerGUID then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (FAssetManagers[LHigh].GetManagerGUID < AAssetManager.GetManagerGUID) then
      Result := LHigh + 1
    else if (FAssetManagers[LLow].GetManagerGUID < AAssetManager.GetManagerGUID) then
      Result := LLow + 1
    else
      Result := LLow;
  end;
var
  LIndex, I: Integer;
begin
  LIndex := GetAssetManagerIndexByGUID(AAssetManager.GetManagerGUID);
  if LIndex = -1 then
  begin
    LIndex := GetSortedPosition;
    SetLength(FAssetManagers, Length(FAssetManagers) + 1);
    // Shift items to the RIGHT
    if LIndex < Length(FAssetManagers) - 1 then
      for I := Length(FAssetManagers) - 1 downto (LIndex + 1) do
      begin
        FAssetManagers[I] := FAssetManagers[I - 1];
        FAssetManagers[I].FIndex := I;
      end;
    // Insert new Event Group
    FAssetManagers[LIndex] := AAssetManager;
    AAssetManager.FIndex := LIndex;
  end;
end;

procedure TLKPackageManager.AddPackage(const APackage: TLKPackage);
  function GetSortedPosition: Integer;
  var
    LIndex, LLow, LHigh: Integer;
  begin
    Result := 0;
    LLow := 0;
    LHigh := Length(FPackages) - 1;
    if LHigh = - 1 then
      Exit;
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if APackage.Name <= FPackages[LIndex].Name then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (FPackages[LHigh].Name < APackage.Name) then
      Result := LHigh + 1
    else if (FPackages[LLow].Name < APackage.Name) then
      Result := LLow + 1
    else
      Result := LLow;
  end;
var
  LIndex, I: Integer;
begin
  LIndex := GetPackageIndexByName(APackage.Name);
  if LIndex = -1 then
  begin
    LIndex := GetSortedPosition;
    SetLength(FPackages, Length(FPackages) + 1);
    // Shift items to the RIGHT
    if LIndex < Length(FPackages) - 1 then
      for I := Length(FPackages) - 1 downto (LIndex + 1) do
      begin
        FPackages[I] := FPackages[I - 1];
        FPackages[I].FIndex := I;
      end;
    // Insert new Event Group
    FPackages[LIndex] := APackage;
    APackage.FIndex := LIndex;
  end;
end;

procedure TLKPackageManager.ClearAssetManagers;
var
  I: Integer;
begin
  for I := High(FAssetManagers) downto Low(FAssetManagers) do
    FAssetManagers[I].Free;
end;

procedure TLKPackageManager.ClearPackages;
var
  I: Integer;
begin
  for I := High(FPackages) downto Low(FPackages) do
    FPackages[I].Free;
end;

constructor TLKPackageManager.Create;
begin
  inherited Create;

end;

procedure TLKPackageManager.DeleteAssetManager(const AAssetManager: TLKAssetManagerBase);
var
  LCount, I: Integer;
begin
  LCount := Length(FAssetManagers);
  if (AAssetManager.FIndex < 0) or (AAssetManager.FIndex > LCount - 1) then
    Exit;
  if (AAssetManager.FIndex < (LCount - 1)) then
    for I := AAssetManager.FIndex to LCount - 2 do
    begin
      FAssetManagers[I] := FAssetManagers[I + 1];
      FAssetManagers[I].FIndex := I;
    end;
  SetLength(FAssetManagers, LCount - 1);
end;

procedure TLKPackageManager.DeletePackage(const APackage: TLKPackage);
var
  LCount, I: Integer;
begin
  LCount := Length(FPackages);
  if (APackage.FIndex < 0) or (APackage.FIndex > LCount - 1) then
    Exit;
  if (APackage.FIndex < (LCount - 1)) then
    for I := APackage.FIndex to LCount - 2 do
    begin
      FPackages[I] := FPackages[I + 1];
      FPackages[I].FIndex := I;
    end;
  SetLength(FPackages, LCount - 1);
end;

destructor TLKPackageManager.Destroy;
begin
  ClearPackages;
  ClearAssetManagers;
  inherited;
end;

function TLKPackageManager.GetAssetManagerByGUID(const AGUID: String): TLKAssetManagerBase;
var
  LIndex: Integer;
begin
  LIndex := GetAssetManagerIndexByGUID(AGUID);
  if LIndex = -1 then
    Result := nil
  else
    Result := FAssetManagers[LIndex];
end;

function TLKPackageManager.GetAssetManagerIndexByGUID(const AGUID: String): Integer;
var
  LIndex, LLow, LHigh: Integer;
begin
  Result := -1;
  LLow := 0;
  LHigh := Length(FAssetManagers) - 1;
  if LHigh > -1 then
  begin
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if AGUID <= FAssetManagers[LIndex].GetManagerGUID then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (FAssetManagers[LHigh].GetManagerGUID = AGUID) then
      Result := LHigh
    else if (FAssetManagers[LLow].GetManagerGUID = AGUID) then
      Result := LLow;
  end;
end;

function TLKPackageManager.GetPackageByName(const AName: String): TLKPackage;
var
  LIndex: Integer;
begin
  LIndex := GetPackageIndexByName(AName);
  if LIndex = -1 then
    Result := nil
  else
    Result := FPackages[LIndex];
end;

function TLKPackageManager.GetPackageIndexByName(const AName: String): TLKPackage;
var
  LIndex, LLow, LHigh: Integer;
begin
  Result := -1;
  LLow := 0;
  LHigh := Length(FPackages) - 1;
  if LHigh > -1 then
  begin
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if AName <= FPackages[LIndex].Name then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (FPackages[LHigh].Name = AName) then
      Result := LHigh
    else if (FPackages[LLow].Name = AName) then
      Result := LLow;
  end;
end;

{ TLKPackage }

procedure TLKPackage.AddAsset(const AAsset: TLKAsset);
begin

end;

procedure TLKPackage.ClearAssets;
var
  I: Integer;
begin
  for I := High(FAssets) to Low(FAssets) do
    FAssets[I].Free;
end;

constructor TLKPackage.Create(const APackageName: String);
begin
  inherited Create;
  Name := APackageName;
  PackageManager.AddPackage(Self);
end;

constructor TLKPackage.Create;
begin
  inherited Create;
  PackageManager.AddPackage(Self);
end;

procedure TLKPackage.DeleteAsset(const AAsset: TLKAsset);
begin

end;

destructor TLKPackage.Destroy;
begin
  ClearAssets;
  PackageManager.DeletePackage(Self);
  inherited;
end;

function TLKPackage.GetAssetByIndex(const AIndex: Integer): TLKAsset;
begin

end;

function TLKPackage.GetAssetByName(const AName: String): TLKAsset;
begin

end;

function TLKPackage.GetAssetIndexByName(const AName: String): Integer;
begin

end;

initialization
  PackageManager := TLKPackageManager.Create;
finalization
  PackageManager.Free;

end.
