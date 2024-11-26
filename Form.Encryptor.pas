unit Form.Encryptor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtDlgs,
  System.IOUtils, System.NetEncoding, System.Hash, System.NetConsts,
  DECCiphers, DECUtil, DECHash, DECCipherBase, Vcl.Menus, Vcl.ExtCtrls, Form.About,
  System.UITypes;

type
  TFormEncryptor = class(TForm)
    MainMenu: TMainMenu;
    MenuItemProgram: TMenuItem;
    SubMenuItemSettings: TMenuItem;
    SubMenuItemExit: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemAbout: TMenuItem;
    btnDecrypt: TButton;
    btnEncrypt: TButton;
    EditPassword1: TEdit;
    EditPassword2: TEdit;
    LabelPassword1: TLabel;
    LabelPassword2: TLabel;
    procedure btnEncryptClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure SubMenuItemExitClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    function Encrypt(const Input: TBytes; const Password: string): TBytes;
    function Decrypt(const Input: TBytes; const Password: string): TBytes;
    function OverwriteFile(const AFileName: string;
      const AValue: TBytes): boolean;
  end;

const
  RS_EXIT_PROGRAM = 'Are you sure to exit the program?';
  RS_ERROR_ENTER_BOTH_PASSWORDS = 'Please enter both passwords!';
  RS_ERROR_GIVEN_FILE_HAS_NO_CONTENT = 'Given file has no content!';
  RS_ERROR_WRONG_PASSWORD = 'Invalid password!';
  RS_SUCCESS_ENCRYPTION_OK = 'File successfully encrypted!';
  RS_SUCCESS_DECRYPTION_OK = 'File successfully decrypted!';
  RS_SELECT_FILE_ENCRYPT = 'Select a file to encrypt';
  RS_SELECT_FILE_DECRYPT = 'Select a file to decrypt';
  ENCRYPTED_MARKER = 'ENCRYPTED';

var
  FormEncryptor: TFormEncryptor;
  FormAbout: TFormAbout;

implementation

{$R *.dfm}

function TFormEncryptor.Encrypt(const Input: TBytes;
  const Password: string): TBytes;
var
  XCipher: TCipher_TwoFish;
  XCipherKey, IV: RawByteString;
begin
  XCipher := TCipher_TwoFish.Create;
  try
    XCipherKey := Password;
    IV := #0#0#0#0#0#0#0#0;

    XCipher.Init(XCipherKey, IV, 0);
    XCipher.Mode := cmCBCx;

    var
    Output := XCipher.EncodeBytes(Input);
    XCipher.Done;
    Result := Output;
  finally
    XCipher.Free;
  end;
end;

procedure TFormEncryptor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := false;
  if MessageDlg(RS_EXIT_PROGRAM, mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then
  begin
    CanClose := true;
  end;
end;

procedure TFormEncryptor.MenuItemAboutClick(Sender: TObject);
begin
  FormAbout := TFormAbout.Create(nil);
  try
    FormAbout.Hide;
    FormAbout.ShowModal;
  finally
    FormAbout.Free;
  end;
end;

function TFormEncryptor.Decrypt(const Input: TBytes;
  const Password: string): TBytes;
var
  XCipher: TCipher_TwoFish;
  XCipherKey, IV: RawByteString;
begin
  XCipher := TCipher_TwoFish.Create;
  try
    XCipherKey := Password;
    IV := #0#0#0#0#0#0#0#0;

    XCipher.Init(XCipherKey, IV, 0);
    XCipher.Mode := cmCBCx;

    var
    Output := XCipher.DecodeBytes(Input);
    XCipher.Done;

    Result := Output;
  finally
    XCipher.Free;
  end;
end;

function TFormEncryptor.OverwriteFile(const AFileName: string;
  const AValue: TBytes): boolean;
var
  FileStream: TFileStream;
begin
  Result := False;
  if AFileName.IsEmpty or (Length(AValue) = 0) then
  begin
    Exit;
  end;
  FileStream := TFileStream.Create(AFileName, fmCreate);
  try
    if (not Assigned(FileStream)) or (FileStream = nil) then
    begin
      Exit;
    end;
    FileStream.WriteBuffer(AValue[0], Length(AValue));
    Result := True;
  finally
    FileStream.Free;
  end;
end;

procedure TFormEncryptor.SubMenuItemExitClick(Sender: TObject);
begin
  Application.MainForm.Close;
end;

procedure TFormEncryptor.btnEncryptClick(Sender: TObject);
var
  xFileDialog: TOpenTextFileDialog;
  xPassword, xPassword2: string;
  xEncPass: TBytes;
  xCon: TBytes;
  xEncCon: TBytes;
  xEncMerged: TBytes;
begin
  xFileDialog := TOpenTextFileDialog.Create(Self);
  xFileDialog.Filter := 'All Files (*.*)|*.*';
  xFileDialog.Title := RS_SELECT_FILE_ENCRYPT;
  xFileDialog.ShowEncodingList := False;

  try
    if xFileDialog.Execute then
    begin
      // Get passwords
      xPassword := EditPassword1.Text;
      xPassword := xPassword.Trim;
      xPassword2 := EditPassword2.Text;
      xPassword2 := xPassword2.Trim;
      if xPassword.IsEmpty or xPassword2.IsEmpty or
        (Length(BytesOf(xPassword)) <= 0) then
      begin
        ShowMessage(RS_ERROR_ENTER_BOTH_PASSWORDS);
        Exit;
      end
      else
      begin
        // Encrypt password
        xEncPass := Encrypt(BytesOf(xPassword), xPassword2);
      end;
      // Get file content
      xCon := TFile.ReadAllBytes(xFileDialog.FileName);
      if Length(xCon) <= 0 then
      begin
        ShowMessage(RS_ERROR_GIVEN_FILE_HAS_NO_CONTENT);
        Exit;
      end
      else
      begin
        // Encrypt content
        xEncCon := Encrypt(xCon, xPassword);
      end;
      // Merge password, content and encrypted marker
      xEncMerged := Concat(xEncPass + BytesOf(sLineBreak),
        xEncCon + BytesOf(sLineBreak), BytesOf(ENCRYPTED_MARKER));
      // Write back to file
      if OverwriteFile(xFileDialog.FileName, xEncMerged) then
      begin
        ShowMessage(RS_SUCCESS_ENCRYPTION_OK);
      end;
    end;
  finally
    xFileDialog.Free;
  end;
end;

procedure TFormEncryptor.btnDecryptClick(Sender: TObject);
var
  xFileDialog: TOpenTextFileDialog;
  xPassword, xPassword2: string;
  xEncMerged: TBytes;
  xEncPass, xEncCon: TBytes;
  xDecPass: TBytes;
  xDecCon: TBytes;
  EncMergedStr, MarkerLine: string;
  LastLineBreak, FirstLineBreak: Integer;
begin
  xFileDialog := TOpenTextFileDialog.Create(Self);
  xFileDialog.Filter := 'All Files (*.*)|*.*';
  xFileDialog.Title := RS_SELECT_FILE_DECRYPT;
  xFileDialog.ShowEncodingList := False;
  try
    if xFileDialog.Execute then
    begin
      // Get passwords
      xPassword := EditPassword1.Text;
      xPassword := xPassword.Trim;
      xPassword2 := EditPassword2.Text;
      xPassword2 := xPassword2.Trim;

      if xPassword.IsEmpty or xPassword2.IsEmpty or
        (Length(BytesOf(xPassword)) <= 0) then
      begin
        ShowMessage(RS_ERROR_ENTER_BOTH_PASSWORDS);
        Exit;
      end;

      // Get file content
      xEncMerged := TFile.ReadAllBytes(xFileDialog.FileName);
      EncMergedStr := TEncoding.Default.GetString(xEncMerged);

      // Find the last line break
      LastLineBreak := LastDelimiter(sLineBreak, EncMergedStr);
      MarkerLine := Trim(Copy(EncMergedStr, LastLineBreak + Length(sLineBreak) -
        1, Length(ENCRYPTED_MARKER)));

      // Check if the marker is correct
      if MarkerLine <> ENCRYPTED_MARKER then
      begin
        Exit;
      end;

      // Remove the marker
      EncMergedStr := Copy(EncMergedStr, 1, LastLineBreak - Length(sLineBreak));

      xEncMerged := TEncoding.Default.GetBytes(EncMergedStr);

      // Find the position of the first line break (between password and content)
      FirstLineBreak := Pos(sLineBreak, EncMergedStr);
      if FirstLineBreak <= 0 then
      begin
        Exit;
      end;

      // Extract the encrypted password
      SetLength(xEncPass, FirstLineBreak - 1);
      Move(xEncMerged[0], xEncPass[0], FirstLineBreak - 1);

      // Extract the encrypted content
      SetLength(xEncCon, Length(xEncMerged) - FirstLineBreak -
        Length(sLineBreak) + 1);
      Move(xEncMerged[FirstLineBreak + Length(sLineBreak) - 1], xEncCon[0],
        Length(xEncCon));

      try
        // Decrypt password
        xDecPass := Decrypt(xEncPass, xPassword2);

        if TEncoding.Default.GetString(xDecPass) <> xPassword then
        begin
          ShowMessage(RS_ERROR_WRONG_PASSWORD);
          Exit;
        end;
      except
        on E: EEncodingError do
        begin
          Exit;
        end;
      end;

      // Decrypt content
      xDecCon := Decrypt(xEncCon, xPassword);

      // Write back to file
      if OverwriteFile(xFileDialog.FileName, xDecCon) then
      begin
        ShowMessage(RS_SUCCESS_DECRYPTION_OK);
      end;
    end;
  finally
    xFileDialog.Free;
  end;
end;

end.
