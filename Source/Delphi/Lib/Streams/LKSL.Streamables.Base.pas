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
unit LKSL.Streamables.Base;

{
  About this unit:
    - This unit is the heart and soul of the "Streamables Engine".
    - This unit MUST be referenced in the "Uses" section of any units intending to use Streamable Types.
    - DON'T FORGET TO CALL "Streamables.Register(<type>);" OR "Streamables.Register([<types]);"
      BEFORE USING ANY OF YOUR STREAMABLE TYPES! THIS CAN BE EASILY DONE IN THE "INITIALIZATION" SECTION.
      YOU CAN THEN CALL "Streamables.Unregister(<type);" OR "Streamables.Unregister([<types]);"
      IN THE "FINALIZATION" SECTION OF YOUR DEFINING UNITS!

  Changelog (latest changes first):
    6th September 2014:
      - Prepared for Release
}

interface

uses
  System.Classes, System.SysUtils,
  LKSL.Common.Types,
  LKSL.Streams.System;

type
  { Forward Declarations }
  TLKStreamable = class;
  TLKStreamableNamed = class;
  TLKStreamableTypes = class;

  { Exceptions }
  // "ELKStreamableException" is the Base Exception Type for ALL Streamables Exceptions
  ELKStreamableException = class(ELKException);
  // "ELKStreamableSignatureMismatch" occurs when a Streamable is being Read from a Stream if the
  // Signatures don't match!
  ELKStreamableSignatureMismatch = class(ELKStreamableException);
  // "ELKStreamableTypesException" is the Base Exception Type for ALL Streamables Types Exceptions
  ELKStreamableTypesException = class(ELKException);
  // "ELKStreamableTypeAlreadyRegistered" occurs when you attempt to register a Streamable Type where
  // a Streamable Type with that GUID has already been registered.
  ELKStreamableTypeAlreadyRegistered = class(ELKStreamableTypesException);
  // "ELKStreamableTypeNotRegistered" occurs when you attempt to UNregister a Streamable Type which has
  // not yet been registered with the Streamable Types Manager.
  ELKStreamableTypeNotRegistered = class(ELKStreamableTypesException);

  { Class References }
  TLKStreamableType = class of TLKStreamable;

  { Array Types }
  TLKStreamableTypeArray = Array of TLKStreamableType;

  {
    TLKStreamable
      - Abstract Base Type for all Streamable Types
  }
  TLKStreamable = class abstract(TLKPersistent)
  private
    FVersion: Double;
  public
    // You MUST provide a UNIQUE GUID String identifier for ALL Streamable Types
    // This GUID is used to uniquely identify a Streamable Type when Reading back Streamables from a Stream.
    // Override "GetTypeGUID" and return a GUID String.
    // TIP: Use Ctrl+Shift+G to generate a GUID, then delete the opening "[" and closing "]".
    class function GetTypeGUID: String; virtual; abstract;
    // "GetTypeVersion" returns a floating point number used to identify a Version of a Streamable Type.
    // By default, it returns a "0", to assume that your type is the FIRST version of its Type.
    // Version numbers can be used to ensure backward-compatibility with earlier serializations of your
    // Streamable Type. You can use an "IF" statement on new additions or changes to ensure that those
    // changed valuesets are only considered if the serialization matches a given version.
    class function GetTypeVersion: Double; virtual;

    constructor Create; override;

    // "DeleteFromStream" removes an instance of your Streamable Type from the given Stream.
    // NOTE: Don't forget to set the starting Position of the Streamable Instance within your Stream first!
    // DON'T FORGET TO CALL "INHERITED;" FIRST
    procedure DeleteFromStream(const AStream: TStream); virtual;
    // "ReadFromStream" reads an instance of your Streamable Type from the given Stream.
    // NOTE: Don't forget to set the starting Position of the Streamable Instance within your Stream first!
    // DON'T FORGET TO CALL "INHERITED;" FIRST
    procedure ReadFromStream(const AStream: TStream); virtual;
    // "InsertIntoStream" inserts an instance of your Streamable Type from the given Stream.
    // NOTE: Don't forget to set the starting Position at which you wish your Streamable Instance to be
    //       Inserted into the Stream
    // DON'T FORGET TO CALL "INHERITED;" FIRST
    procedure InsertIntoStream(const AStream: TStream); virtual;
    // "WriteToStream" writes an instance of your Streamable Type from the given Stream.
    // NOTE: It is always added to the END of the given Stream.
    // DON'T FORGET TO CALL "INHERITED;" FIRST
    procedure WriteToStream(const AStream: TStream); virtual;

    // "LoadFromFile" sets the Member Data of your Streamable Instance from a saved File
    procedure LoadFromFile(const AFileName: String);
    // "LoadFromsTream" sets the Member Data of your Streamable Instance from a Stream
    // It is essentially an ALIAS of "ReadFromStream"
    // NOTE: Don't forget to set the starting Position of the Streamable Instance within your Stream first!
    procedure LoadFromStream(const AStream: TStream);
    // "SaveToFile" saves the Member Data of your Streamable Instance to a File
    procedure SaveToFile(const AFileName: String);
    // "SaveToStream" WRITES the Member Data of your Streamable Instance to the END of the given Stream.
    // It is essentially an ALIAS of "WriteToStream"
    procedure SaveToStream(const AStream: TStream);

    property Version: Double read FVersion;
  end;

  {
    TLKStreamableNamed
      - Abstract Type for all NAMED Streamable Types
      - Basically it adds a "Name" property and handles the Deleting, Inserting, Reading and Writing of
        that name to/from Streams.
  }
  TLKStreamableNamed = class abstract(TLKStreamable)
  private
    FName: String;
    function GetName: String;
    procedure SetName(const AName: String);
  public
    procedure DeleteFromStream(const AStream: TStream); override;
    procedure ReadFromStream(const AStream: TStream); override;
    procedure InsertIntoStream(const AStream: TStream); override;
    procedure WriteToStream(const AStream: TStream); override;
    property Name: String read GetName write SetName;
  end;

  {
    TLKStreamableTypes
      - The "Streamable Types Manager"
      - All Streamable Types should be Registered against the "Streamables" Manager so that they can be
        requested when reading a serialization of a Streamable Instance from a Stream.
      - This Manager allows you to request the Class Type Reference of a Streamable from within a Stream
        so that it can be constructed dynamically at runtime.
        This means that you can store arbitrary Streamable Type Instances within a Stream, and the correct
        Class Instance for that Streamable Type will be constructed when you're loading from a Stream.

      THIS STREAMABLES MANAGER IS 100% THREAD-SAFE!
  }
  TLKStreamableTypes =  class(TLKPersistent)
  private
    FStreamableTypes: TLKStreamableTypeArray;

    procedure Clear;
    function GetCount: Integer;
    function GetStreamableTypeByIndex(const AIndex: Integer): TLKStreamableType;
    function GetStreamableTypeByGUID(const AGUID: String): TLKStreamableType;
    function GetStreamableTypeIndexByGUID(const AGUID: String): Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Register(const AStreamableType: TLKStreamableType); overload;
    procedure Register(const AStreamableTypes: Array of TLKStreamableType); overload;
    procedure Unregister(const AStreamableType: TLKStreamableType); overload;
    procedure Unregister(const AStreamableTypes: Array of TLKStreamableType); overload;

    function GetTypeFromStream(const AStream: TStream): TLKStreamableType;

    property Count: Integer read GetCount;
    property StreamableType[const AIndex: Integer]: TLKStreamableType read GetStreamableTypeByIndex; default;
    property StreamableType[const AGUID: String]: TLKStreamableType read GetStreamableTypeByGUID; default;
  end;

var
  Streamables: TLKStreamableTypes;

implementation

{ TLKStreamable }

constructor TLKStreamable.Create;
begin
  inherited;
  FVersion := GetTypeVersion;
end;

procedure TLKStreamable.DeleteFromStream(const AStream: TStream);
begin
  StreamDeleteString(AStream); // Remove the GUID
  StreamDeleteDouble(AStream); // Remove the Version
end;

class function TLKStreamable.GetTypeVersion: Double;
begin
  Result := 0.00; // We assume that this is the FRIST version of this Streamable Type
end;

procedure TLKStreamable.InsertIntoStream(const AStream: TStream);
begin
  StreamInsertString(AStream, GetTypeGUID); // Insert the GUID
  StreamInsertDouble(AStream, GetTypeVersion); // Insert the Version
end;

procedure TLKStreamable.LoadFromFile(const AFileName: String);
var
  LStream: TStream;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TLKStreamable.LoadFromStream(const AStream: TStream);
begin
  ReadFromStream(AStream);
end;

procedure TLKStreamable.ReadFromStream(const AStream: TStream);
var
  LSignature: String;
begin
  LSignature := StreamReadString(AStream); // Read the GUID
  if LSignature = GetTypeGUID then // Check if the Signature matches the expected GUID
  begin
    FVersion := StreamReadDouble(AStream); // Read the Version
  end else
  begin
    raise ELKStreamableSignatureMismatch.CreateFmt('Stream Signature Mismatch! Expected "%s", got "%s', [GetTypeGUID, LSignature]);
  end;
end;

procedure TLKStreamable.SaveToFile(const AFileName: String);
var
  LStream: TStream;
begin
  LStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TLKStreamable.SaveToStream(const AStream: TStream);
begin
  WriteToStream(AStream);
end;

procedure TLKStreamable.WriteToStream(const AStream: TStream);
begin
  StreamWriteString(AStream, GetTypeGUID); // Write the GUID
  StreamWriteDouble(AStream, GetTypeVersion); // Write the Version
end;

{ TLKStreamableNamed }

procedure TLKStreamableNamed.DeleteFromStream(const AStream: TStream);
begin
  Lock;
  inherited;
  StreamDeleteString(AStream); // Delete Name
  Unlock;
end;

function TLKStreamableNamed.GetName: String;
begin
  Lock;
  Result := FName;
  Unlock;
end;

procedure TLKStreamableNamed.InsertIntoStream(const AStream: TStream);
begin
  Lock;
  inherited;
  StreamInsertString(AStream, FName); // Insert Name
  Unlock;
end;

procedure TLKStreamableNamed.ReadFromStream(const AStream: TStream);
begin
  Lock;
  inherited;
  FName := StreamReadString(AStream);
  Unlock;
end;

procedure TLKStreamableNamed.SetName(const AName: String);
begin
  Lock;
  FName := AName;
  Unlock;
end;

procedure TLKStreamableNamed.WriteToStream(const AStream: TStream);
begin
  Lock;
  inherited;
  StreamWriteString(AStream, FName);
  Unlock;
end;

{ TLKStreamableTypes }

procedure TLKStreamableTypes.Clear;
begin
  Lock;
  SetLength(FStreamableTypes, 0);
  Unlock;
end;

constructor TLKStreamableTypes.Create;
begin
  inherited
end;

destructor TLKStreamableTypes.Destroy;
begin
  Clear;
  inherited;
end;

function TLKStreamableTypes.GetCount: Integer;
begin
  Lock;
  Result := Length(FStreamableTypes);
  Unlock;
end;

function TLKStreamableTypes.GetStreamableTypeByGUID(const AGUID: String): TLKStreamableType;
var
  LIndex: Integer;
begin
  Lock;
  LIndex := GetStreamableTypeIndexByGUID(AGUID);
  if LIndex = -1 then
    Result := nil
  else
    Result := FStreamableTypes[LIndex];
  Unlock;
end;

function TLKStreamableTypes.GetStreamableTypeByIndex(const AIndex: Integer): TLKStreamableType;
begin
  Lock;
  Result := FStreamableTypes[AIndex];
  Unlock;
end;

function TLKStreamableTypes.GetStreamableTypeIndexByGUID(const AGUID: String): Integer;
var
  LIndex, LLow, LHigh: Integer;
begin
  Lock;
  Result := -1;
  LLow := 0;
  LHigh := Length(FStreamableTypes) - 1;
  if LHigh > -1 then
  begin
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if AGUID <= FStreamableTypes[LIndex].GetTypeGUID then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (FStreamableTypes[LHigh].GetTypeGUID = AGUID) then
      Result := LHigh
    else if (FStreamableTypes[LLow].GetTypeGUID = AGUID) then
      Result := LLow;
  end;
  Unlock;
end;

function TLKStreamableTypes.GetTypeFromStream(const AStream: TStream): TLKStreamableType;
var
  LPosition: Int64;
  LSignature: String;
begin
  LPosition := AStream.Position;
  LSignature := StreamReadString(AStream);
  Result := GetStreamableTypeByGUID(LSignature);
  AStream.Position := LPosition;
end;

procedure TLKStreamableTypes.Register(const AStreamableTypes: array of TLKStreamableType);
var
  I: Integer;
begin
  for I := Low(AStreamableTypes) to High(AStreamableTypes) do
    Register(AStreamableTypes[I]);
end;

procedure TLKStreamableTypes.Register(const AStreamableType: TLKStreamableType);
  function GetSortedPosition(const AGUID: String): Integer;
  var
    LIndex, LLow, LHigh: Integer;
  begin
    Result := 0;
    LLow := 0;
    LHigh := Length(FStreamableTypes) - 1;
    if LHigh = - 1 then
      Exit;
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if AGUID <= FStreamableTypes[LIndex].GetTypeGUID then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (FStreamableTypes[LHigh].GetTypeGUID < AGUID) then
      Result := LHigh + 1
    else if (FStreamableTypes[LLow].GetTypeGUID < AGUID) then
      Result := LLow + 1
    else
      Result := LLow;
  end;
var
  I, LIndex: Integer;
begin
  Lock;
  LIndex := GetStreamableTypeIndexByGUID(AStreamableType.GetTypeGUID);
  if LIndex = -1 then
  begin
    LIndex := GetSortedPosition(AStreamableType.GetTypeGUID);
    SetLength(FStreamableTypes, Length(FStreamableTypes) + 1);
    // Shift elements RIGHT
    if LIndex < Length(FStreamableTypes) - 1 then
      for I := Length(FStreamableTypes) - 1 downto LIndex + 1 do
      begin
        FStreamableTypes[I] := FStreamableTypes[I - 1];
      end;
    // Insert new item now
    FStreamableTypes[LIndex] := AStreamableType;
  end else
  begin
    raise ELKStreamableTypeAlreadyRegistered.CreateFmt('A Streamable Type with the GUID "%s" is already registered', [AStreamableType.GetTypeGUID]);
  end;
  Unlock;
end;

procedure TLKStreamableTypes.Unregister(const AStreamableTypes: array of TLKStreamableType);
var
  I: Integer;
begin
  for I := Low(AStreamableTypes) to High(AStreamableTypes) do
    Unregister(AStreamableTypes[I]);
end;

procedure TLKStreamableTypes.Unregister(const AStreamableType: TLKStreamableType);
var
 LIndex: Integer;
 LCount, I: Integer;
begin
  Lock;
  LIndex := GetStreamableTypeIndexByGUID(AStreamableType.GetTypeGUID);
  if LIndex > -1 then
  begin
    LCount := Length(FStreamableTypes);
    if (LIndex < 0) or (LIndex > LCount - 1) then
      Exit;
    if (LIndex < (LCount - 1)) then
      for I := LIndex to LCount - 2 do
      begin
        FStreamableTypes[I] := FStreamableTypes[I + 1];
      end;
    SetLength(FStreamableTypes, LCount - 1);
  end else
  begin
    raise ELKStreamableTypeNotRegistered.CreateFmt('A Streamable Type with the GUID "%s" has not yet been registered, therefore cannot be unregistered!', [AStreamableType.GetTypeGUID]);
  end;
  Unlock;
end;

initialization
  Streamables := TLKStreamableTypes.Create;
finalization
  Streamables.Free;

end.
