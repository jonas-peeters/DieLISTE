unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, serverAPI;

type
  TForm6 = class(TForm)
    ContentPanel: TPanel;
    NavigationPanel: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;
  serverAPI: TServerAPI;

implementation

{$R *.fmx}

end.
