unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, RtfToPdf;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  try
    RtfToPdf.ExecRtfToPdf(Edit1.Text, Edit2.Text);
    ShowMessage('Conversion completed successfully');
  except
    on E: EFileNotFound do
      ShowMessage('Error: ' + E.Message);
    on E: EConversionErr do
      ShowMessage('Error: ' + E.Message);

    on E: EInvalidJsonResponse do
      ShowMessage('Error: ' + E.Message);
    on E: EUnknownStatus do
      ShowMessage('Error: ' + E.Message);
    on E: EInvalidArgLength do
      ShowMessage('Error: ' + E.Message);
    on E: EUnknownErrType do
      ShowMessage('Error: ' + E.Message);

    on E: Exception do
      ShowMessage('Error: ' + E.Message);
  end;
end;

end.
