program TestFileComm;

uses
  Forms,
  FormTestFileComm in 'FormTestFileComm.pas' {Form1},
  DH_FileComm in 'DH_FileComm.pas',
  DH_BasisRoutinen in '..\SPC\System\DH_BasisRoutinen.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
