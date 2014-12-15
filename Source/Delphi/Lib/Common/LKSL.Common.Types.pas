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
unit LKSL.Common.Types;

{$I LKSL.inc}

{$IFNDEF LKSL_SUPPRESS_VERSION_WARNING}
  {$IFNDEF DELPHIXE2}
    {$MESSAGE WARN 'Delphi 2010 and XE are not regularly tested with the LKSL. Please report any issues on https://github.com/LaKraven/LKSL'}
    {$DEFINE LKSL_WARNING_VERSION}
  {$ENDIF DELPHIXE2}

  {$IFDEF DELPHIXE8}
    {$MESSAGE WARN 'Delphi XE8 is in Beta, problems may be bugs in Delphi itself! Please report any issues on https://github.com/LaKraven/LKSL'}
    {$DEFINE LKSL_WARNING_VERSION}
  {$ENDIF DELPHIXE8}
{$ENDIF LKSL_SUPPRESS_VERSION_WARNING}

{$IFNDEF LKSL_SUPPRESS_DEPRECATION_WARNING}
  {$IFNDEF LKSL_USE_LISTS}
    {$MESSAGE WARN 'The use of Generic Lists is going to become mandatory by January 2015. If you object to this, please contact us through https://github.com/LaKraven/LKSL'}
    {$DEFINE LKSL_WARNING_DEPRECATION}
  {$ENDIF LKSL_USE_LISTS}
{$ENDIF LKSL_SUPPRESS_DEPRECATION_WARNING}

{$IFDEF LKSL_WARNING_VERSION}
  {$MESSAGE HINT 'Define "LKSL_SUPPRESS_VERSION_WARNING" in your project options to get rid of these messages'}
  {$UNDEF LKSL_WARNING_VERSION}
{$ENDIF LKSL_WARNING_VERSION}

{$IFDEF LKSL_WARNING_DEPRECATION}
  {$MESSAGE HINT 'Define "LKSL_SUPPRESS_DEPRECATION_WARNING" in your project options to get rid of these messages'}
{$ENDIF LKSL_WARNING_DEPRECATION}

{
  About this unit:
    - This unit provides fundamental abstract base types used throughout the LKSL
}

interface

uses
  {$IFDEF LKSL_USE_EXPLICIT_UNIT_NAMES}
    System.Classes, System.SysUtils, System.SyncObjs;
  {$ELSE}
    Classes, SysUtils, SyncObjs;
  {$ENDIF LKSL_USE_EXPLICIT_UNIT_NAMES}

  {$I LKSL_RTTI.inc}

type
  { Forward Declarations }
  TLKPersistent = class;
  TLKObject = class;

  { Exception Types }
  ELKException = class(Exception);

  {
    TLKPersistent
      - Provides a "Critical Section" (or "Lock") to make members "Thread-Safe"
  }
  TLKPersistent = class(TPersistent)
  private
    FLock: TCriticalSection;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Lock; inline;
    procedure Unlock; inline;
  end;

  {
    TLKObject
      - Provides a "Critical Section" (or "Lock") to make members "Thread-Safe"
  }
  TLKObject = class(TObject)
  private
    FLock: TCriticalSection;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Lock; inline;
    procedure Unlock; inline;
  end;

implementation

{ TLKPersistent }

constructor TLKPersistent.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TLKPersistent.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TLKPersistent.Lock;
begin
  FLock.Acquire;
end;

procedure TLKPersistent.Unlock;
begin
  FLock.Release;
end;

{ TLKObject }

constructor TLKObject.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TLKObject.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TLKObject.Lock;
begin
  FLock.Acquire;
end;

procedure TLKObject.Unlock;
begin
  FLock.Release;
end;

end.
