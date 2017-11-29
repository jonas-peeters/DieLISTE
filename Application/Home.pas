unit Home;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, ServerAPI, FMX.Layouts,
  FMX.ListBox, Supermaerkte, SupermarktListe, Profil;

type
  TForm1 = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    PlusBtn2: TButton;
    ProfilBtn2: TButton;
    HomeBtn2: TButton;
    SupermaerkteBtn2: TButton;
    Label1: TLabel;
    EditBtn2: TButton;
    ListBox1: TListBox;
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

end.
