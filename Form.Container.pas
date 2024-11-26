unit Form.Container;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Form.Encryptor, Form.About, Vcl.Menus,
  System.UITypes, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormContainer = class(TForm)
    MainMenu: TMainMenu;
    MenuItemProgram: TMenuItem;
    SubMenuItemSettings: TMenuItem;
    SubMenuItemExit: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemAbout: TMenuItem;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SubMenuItemExitClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function SetForm(AFormClass: TFormClass; AForm: TForm;
      AVisible: Boolean): TForm;
    procedure CloseActiveForm();

  end;

const
  RS_EXIT_PROGRAM = 'Are you sure to exit the program?';

var
  FormContainer: TFormContainer;
  FormEncryptor: TFormEncryptor;
  FormAbout: TFormAbout;
  ActiveForm: TForm;

implementation

{$R *.dfm}

procedure TFormContainer.FormShow(Sender: TObject);
begin
  CloseActiveForm();
  ActiveForm := SetForm(TFormEncryptor, FormEncryptor, true);
  FormEncryptor := TFormEncryptor(ActiveForm);
end;

procedure TFormContainer.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := false;
  if MessageDlg(RS_EXIT_PROGRAM, mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then
  begin
    CanClose := true;
  end;
end;

procedure TFormContainer.MenuItemAboutClick(Sender: TObject);
begin
  FormAbout := TFormAbout.Create(nil);
  try
    FormAbout.Hide;
    FormAbout.ShowModal;
  finally
    FormAbout.Free;
  end;
end;

procedure TFormContainer.SubMenuItemExitClick(Sender: TObject);
begin
  Application.MainForm.Close;
end;

// Form based methods
procedure TFormContainer.CloseActiveForm();
begin
  if Assigned(ActiveForm) or (ActiveForm <> nil) then
  begin
    ActiveForm.Close;
    FreeAndNil(ActiveForm);
  end;
end;

function TFormContainer.SetForm(AFormClass: TFormClass; AForm: TForm;
  AVisible: Boolean): TForm;
begin
  if not Assigned(AForm) or (AForm = nil) then
  begin
    AForm := AFormClass.Create(Self);
  end;

  if AForm.Visible then
  begin
    if not AVisible then
    begin
      AForm.Close;
    end
    else
    begin
      AForm.BorderStyle := bsNone;
      //Width := AForm.Width;
      //Height := AForm.Height;
      //ClientWidth := AForm.ClientWidth;
      //ClientHeight := AForm.ClientHeight;
    end;
  end;

  Result := AForm;
end;

end.
