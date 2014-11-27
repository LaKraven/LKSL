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
    27th November 2014:
      - Added Class Procedure "Register" to TLKStreamable
        - It's basically an alias of "Streamables.Register(Self);"
      - Added Constructor "CreateFromStream" to TLKStreamable
        - Takes a TStream reference, and initialzies the object using the serialization in the Stream
      - Added Constructor "CreateFromFile" to TLKStreamable
        - Takes a Filename and initializes the object using the serialization in that File
      - Added function "CreateStreamableFromStream" Params: (AStream: TStream; APosition: Int64 [optional])
        - Returns a new instance of the appropriate Streamable Type populated from the Stream
        - Returns a "nil" if the signature does not match a registered Streamable Type
      - TLKStreamable significant INTERFACE-BREAKING changes:
        > For the sake of implementation overrides:
          - (Public) "DeleteFromStream" became (Protected) "RemoveFromStream"
          - (Public) "ReadFromStream" became (Protected) "ReadFromStream"
          - (Public) "InsertIntoStream" became (Protected) "InsertIntoStream"
          - (Public) "WriteToStream" became (Protected) "WriteToStream"
        > For the sake of external calls to TLKStreamable descendents:
          - "DeleteFromStream" remains the same (only the implementation has been relocated)
          - "LoadFromStream" replaces "ReadFromStream" for external reference
          - "SaveToStream" replaces "WriteToStream" and "InsertIntoSTream" for external reference
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
  TLKStreamables = class;

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
  TLKStreamableArray = Array of TLKStreamable;
  TLKStreamableTypeArray = Array of TLKStreamableType;

  {
    TLKStreamable
      - Abstract Base Type for all Streamable Types
  }
  TLKStreamable = class abstract(TLKPersistent)
  private
    FVersion: Double;
  protected
    // "ReadFromStream" reads an instance of your Streamable Type from the given Stream.
    // NOTE: Don't forget to set the starting Position of the Streamable Instance within your Stream first!
    // NOTE: The object's thread-safe LOCK is engaged for this call! No need to call "Lock" in your implementation!
    procedure ReadFromStream(const AStream: TStream); virtual; abstract;
    // "RemoveFromStream" needs to remove an instance of your Streamable Type from the given Stream.
    // NOTE: The object's thread-safe LOCK is engaged for this call! No need to call "Lock" in your implementation!
    class procedure RemoveFromStream(Const AStream: TStream); virtual; abstract;
    // "InjectIntoStream" inserts an instance of your Streamable Type into the given Stream.
    // NOTE: The object's thread-safe LOCK is engaged for this call! No need to call "Lock" in your implementation!
    procedure InsertIntoStream(const AStream: TStream); virtual; abstract;
    // "WriteToStream" writes an instance of your Streamable Type to the END of the given Stream.
    // NOTE: The object's thread-safe LOCK is engaged for this call! No need to call "Lock" in your implementation!
    procedure WriteToStream(const AStream: TStream); virtual; abstract;
  public
    // Creates a BLANK instance of your Streamable
    constructor Create; override;
    // Creates a POPULATED instance of your Streamable from a Stream
    constructor CreateFromStream(const AStream: TStream); overload;
    constructor CreateFromStream(const AStream: TStream; const APosition: Int64); overload;
    // Creates a POPULATED instance of your Streamable from a File
    constructor CreateFromFile(const AFileName: String);
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
    // "Register" will register this Streamable Type with the "Streamable Types Manager"
    // This means you can request the appropriate Streamable Type using its Type GUID String from a
    // Stream.
    // NOTE: You should ALWAYS Register TLKStreamable descendants (but NOT Abstract descendants).
    //       You can call "Streamables.Register" and pass an ARRAY of TLKStreamable descendants too!
    class procedure Register;
    class procedure Unregister;
    // "DeleteFromStream" removes an instance of your Streamable Type from the given Stream.
    // NOTE: Don't forget to set the starting Position of the Streamable Instance within your Stream first!
    // DON'T FORGET TO CALL "INHERITED;" FIRST
    // NOTE: "DeleteFromStream" MUST be a CLASS method!
    //       Remember that a Stream knows how large each data block is, and the Stream Handlers know how
    //       much memory to clear automagically.
    class procedure DeleteFromStream(const AStream: TStream);
    // "LoadFromFile" sets the Member Data of your Streamable Instance from a saved File
    procedure LoadFromFile(const AFileName: String);
    // "LoadFromStream" sets the Member Data of your Streamable Instance from a Stream
    // It is essentially an ALIAS of "ReadFromStream"
    // NOTE: Don't forget to set the starting Position of the Streamable Instance within your Stream first!
    procedure LoadFromStream(const AStream: TStream); overload;
    procedure LoadFromSTream(const AStream: TStream; const APosition: Int64); overload; inline;
    // "SaveToFile" saves the Member Data of your Streamable Instance to a File
    procedure SaveToFile(const AFileName: String);
    // "SaveToStream" WRITES the Member Data of your Streamable Instance to the END of the given Stream.
    // It is essentially an ALIAS of "WriteToStream"
    procedure SaveToStream(const AStream: TStream); overload;
    procedure SaveToStream(const AStream: TStream; const APosition: Int64); overload;

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
  protected
    class procedure RemoveFromStream(const AStream: TStream); override;
    procedure ReadFromStream(const AStream: TStream); override;
    procedure InsertIntoStream(const AStream: TStream); override;
    procedure WriteToStream(const AStream: TStream); override;
  public
    property Name: String read GetName write SetName;
  end;

  {
    TLKStreamables
      - The "Streamable Types Manager"
      - All Streamable Types should be Registered against the "Streamables" Manager so that they can be
        requested when reading a serialization of a Streamable Instance from a Stream.
      - This Manager allows you to request the Class Type Reference of a Streamable from within a Stream
        so that it can be constructed dynamically at runtime.
        This means that you can store arbitrary Streamable Type Instances within a Stream, and the correct
        Class Instance for that Streamable Type will be constructed when you're loading from a Stream.

      THIS STREAMABLES MANAGER IS 100% THREAD-SAFE!
  }
  TLKStreamables =  class(TLKPersistent)
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

    function CreateStreamableFromStream(const AStream: TStream): TLKStreamable; overload;
    function CreateStreamableFromStream(const AStream: TStream; const APosition: Int64): TLKStreamable; overload;
    function GetStreamableTypeFromStream(const AStream: TStream): TLKStreamableType;
    function StreamableTypeMatch(const AStream: TStream; const AStreamableType: TLKStreamableType): Boolean;

    procedure DeleteArrayOfStreamables(const AStream: TStream);
    procedure InsertArrayOfStreamables(const AStream: TStream; const AStreamables: TLKStreamableArray);
    procedure WriteArrayOfStreamables(const AStream: TStream; const AStreamables: TLKStreamableArray);

    property Count: Integer read GetCount;
    property StreamableType[const AIndex: Integer]: TLKStreamableType read GetStreamableTypeByIndex; default;
    property StreamableType[const AGUID: String]: TLKStreamableType read GetStreamableTypeByGUID; default;
  end;

var
  Streamables: TLKStreamables;

implementation

{ TLKStreamable }

constructor TLKStreamable.Create;
begin
  inherited;
  FVersion := GetTypeVersion;
end;

constructor TLKStreamable.CreateFromStream(const AStream: TStream);
begin
  inherited Create;
  ReadFromStream(AStream);
end;

constructor TLKStreamable.CreateFromFile(const AFileName: String);
begin
  inherited Create;
  LoadFromFile(AFileName);
end;

constructor TLKStreamable.CreateFromStream(const AStream: TStream; const APosition: Int64);
begin
  AStream.Position := APosition;
  CreateFromStream(AStream);
end;

class procedure TLKStreamable.DeleteFromStream(const AStream: TStream);
begin
  StreamDeleteString(AStream); // Remove the GUID
  StreamDeleteDouble(AStream); // Remove the Version
  RemoveFromStream(AStream); // Remove the descendant's custom members
end;

class function TLKStreamable.GetTypeVersion: Double;
begin
  Result := 0.00; // We assume that this is the FRIST version of this Streamable Type
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

procedure TLKStreamable.LoadFromStream(const AStream: TStream; const APosition: Int64);
begin
  AStream.Position := APosition;
  LoadFromStream(AStream);
end;

procedure TLKStreamable.LoadFromStream(const AStream: TStream);
var
  LSignature: String;
begin
  Lock;
  try
    LSignature := StreamReadString(AStream); // Read the GUID
    if LSignature = GetTypeGUID then // Check if the Signature matches the expected GUID
    begin
      FVersion := StreamReadDouble(AStream); // Read the Version
      ReadFromStream(AStream); // Read this type's specific values
    end else
    begin
      raise ELKStreamableSignatureMismatch.CreateFmt('Stream Signature Mismatch! Expected "%s", got "%s', [GetTypeGUID, LSignature]);
    end;
  finally
    Unlock;
  end;
end;

class procedure TLKStreamable.Register;
begin
  Streamables.Register(Self);
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

procedure TLKStreamable.SaveToStream(const AStream: TStream; const APosition: Int64);
begin
  if APosition = AStream.Size then
    SaveToStream(AStream)
  else
  begin
    Lock;
    try
      StreamInsertString(AStream, GetTypeGUID, APosition);
      StreamInsertDouble(AStream, GetTypeVersion);
      InsertIntoStream(AStream);
    finally
      Unlock;
    end;
  end;
end;

class procedure TLKStreamable.Unregister;
begin
  Streamables.Unregister(Self);
end;

procedure TLKStreamable.SaveToStream(const AStream: TStream);
begin
  Lock;
  try
    StreamWriteString(AStream, GetTypeGUID); // Write the GUID
    StreamWriteDouble(AStream, GetTypeVersion); // Write the Version
    WriteToStream(AStream);
  finally
    Unlock;
  end;
end;

{ TLKStreamableNamed }

class procedure TLKStreamableNamed.RemoveFromStream(const AStream: TStream);
begin
  inherited;
  StreamDeleteString(AStream); // Delete Name
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

{ TLKStreamables }

procedure TLKStreamables.Clear;
begin
  Lock;
  SetLength(FStreamableTypes, 0);
  Unlock;
end;

constructor TLKStreamables.Create;
begin
  inherited
end;

function TLKStreamables.CreateStreamableFromStream(const AStream: TStream): TLKStreamable;
begin
  Result := CreateStreamableFromStream(AStream, AStream.Position);
end;

function TLKStreamables.CreateStreamableFromStream(const AStream: TStream; const APosition: Int64): TLKStreamable;
var
  LStreamableType: TLKStreamableType;
begin
  AStream.Position := APosition;
  LStreamableType := GetStreamableTypeFromStream(AStream);
  if LStreamableType = nil then
    Result := nil
  else
    Result := LStreamableType.CreateFromStream(AStream);
end;

procedure TLKStreamables.DeleteArrayOfStreamables(const AStream: TStream);
var
  I, LCount: Integer;
begin
  I := AStream.Position; // Store the initial Position locally
  LCount := StreamReadInteger(AStream); // Read the Array Count from the Stream
  AStream.Position := I; // Reposition the Stream back before the Array Count
  StreamDeleteInteger(AStream); // Delete the Array Count from the Stream
  for I := 0 to LCount - 1 do // Iterate the Array
    GetStreamableTypeFromStream(AStream).DeleteFromStream(AStream); // Delete Array Item from the Stream
end;

destructor TLKStreamables.Destroy;
begin
  Clear;
  inherited;
end;

function TLKStreamables.GetCount: Integer;
begin
  Lock;
  Result := Length(FStreamableTypes);
  Unlock;
end;

function TLKStreamables.GetStreamableTypeByGUID(const AGUID: String): TLKStreamableType;
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

function TLKStreamables.GetStreamableTypeByIndex(const AIndex: Integer): TLKStreamableType;
begin
  Lock;
  Result := FStreamableTypes[AIndex];
  Unlock;
end;

function TLKStreamables.GetStreamableTypeIndexByGUID(const AGUID: String): Integer;
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

function TLKStreamables.GetStreamableTypeFromStream(const AStream: TStream): TLKStreamableType;
var
  LPosition: Int64;
  LSignature: String;
begin
  LPosition := AStream.Position;
  LSignature := StreamReadString(AStream);
  Result := GetStreamableTypeByGUID(LSignature);
  AStream.Position := LPosition;
end;

procedure TLKStreamables.InsertArrayOfStreamables(const AStream: TStream; const AStreamables: TLKStreamableArray);
var
  I: Integer;
begin
  StreamInsertInteger(AStream, Length(AStreamables)); // Insert the Array Count into the Stream
  for I := Low(AStreamables) to High(AStreamables) do // Iterate the Array
    AStreamables[I].InsertInToStream(AStream); // Insert the Item into the Stream
end;

procedure TLKStreamables.Register(const AStreamableTypes: array of TLKStreamableType);
var
  I: Integer;
begin
  for I := Low(AStreamableTypes) to High(AStreamableTypes) do
    Register(AStreamableTypes[I]);
end;

function TLKStreamables.StreamableTypeMatch(const AStream: TStream; const AStreamableType: TLKStreamableType): Boolean;
var
  LStreamableType: TLKStreamableType;
begin
  LStreamableType := GetStreamableTypeFromStream(AStream);
  Result := (((LStreamableType <> nil)) and (LStreamableType = AStreamableType));
end;

procedure TLKStreamables.Register(const AStreamableType: TLKStreamableType);
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

procedure TLKStreamables.Unregister(const AStreamableTypes: array of TLKStreamableType);
var
  I: Integer;
begin
  for I := Low(AStreamableTypes) to High(AStreamableTypes) do
    Unregister(AStreamableTypes[I]);
end;

procedure TLKStreamables.WriteArrayOfStreamables(const AStream: TStream; const AStreamables: TLKStreamableArray);
var
  I: Integer;
begin
  StreamWriteInteger(AStream, Length(AStreamables)); // Write the Array Count into the Stream
  for I := Low(AStreamables) to High(AStreamables) do // Iterate the Array
    AStreamables[I].SaveToStream(AStream); // Write the Item into the Stream
end;

procedure TLKStreamables.Unregister(const AStreamableType: TLKStreamableType);
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
  Streamables := TLKStreamables.Create;
finalization
  Streamables.Free;

end.
