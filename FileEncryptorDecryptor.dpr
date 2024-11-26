program FileEncryptorDecryptor;

uses
  Vcl.Forms,
  Form.Encryptor in 'Form.Encryptor.pas' {FormEncryptor},
  Form.About in 'Form.About.pas' {FormAbout},
  Form.Container in 'Form.Container.pas' {FormContainer};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormEncryptor, FormEncryptor);
  Application.Run;
end.
