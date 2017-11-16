unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, ServerAPI;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  serverAPI: TServerAPI;

implementation

{$R *.fmx}
{$R *.Macintosh.fmx MACOS}

procedure TForm1.Button1Click(Sender: TObject);
begin
  serverAPI := TServerAPI.create();
  memo1.lines.add(serverAPI.login('jonas.peeters@icloud.com', '1234'));
  memo1.lines.add(serverAPI.me());
end;

end.
