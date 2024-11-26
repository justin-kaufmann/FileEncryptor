unit Form.About;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormAbout = class(TForm)
    ButtonClose: TButton;
    MemoAbout: TMemo;
    procedure ButtonCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormAbout.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

end.
