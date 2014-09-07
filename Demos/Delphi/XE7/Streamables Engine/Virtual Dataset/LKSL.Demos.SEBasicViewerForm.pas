unit LKSL.Demos.SEBasicViewerForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects;

type
  TfrmImageViewer = class(TForm)
    imgViewer: TImage;
  private
    { Private declarations }
  public
    procedure SetImage(const ABitmap: TBitmap);
  end;

var
  frmImageViewer: TfrmImageViewer;

implementation

{$R *.fmx}

{ TfrmImageViewer }

procedure TfrmImageViewer.SetImage(const ABitmap: TBitmap);
begin
  imgViewer.Bitmap.Assign(ABitmap);
end;

end.
