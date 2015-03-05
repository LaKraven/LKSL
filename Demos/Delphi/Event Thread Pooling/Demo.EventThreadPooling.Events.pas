unit Demo.EventThreadPooling.Events;

interface

uses
  System.Classes,
  LKSL.Common.Types,
  LKSL.Events.Main;

type
  TTestEvent = class(TLKEvent)
  private
    FFoo: String;
  public
    constructor Create(const AFoo: String); reintroduce;
    property Foo: String read FFoo;
  end;

  TTestEventListener = class(TLKEventListener<TTestEvent>);

implementation

{ TTestEvent }

constructor TTestEvent.Create(const AFoo: String);
begin
  inherited Create;
  FFoo := AFoo;
end;

end.
