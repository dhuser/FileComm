unit FormTestFileComm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, DateUtils, 

  DH_Basisroutinen, DH_FileComm, StdCtrls;


type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    butSendeN1: TButton;
    Memo1: TMemo;
    butLoeschen: TButton;
    butSende1N10: TButton;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure butSendeN1Click(Sender: TObject);
    procedure butLoeschenClick(Sender: TObject);
    procedure butSende1N10Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    Comm: array[0..2] of TFileComm;
    Clients: array[0..2] of TFileComm;
    AntwortZaehler: integer;
    SendeZaehler: integer;

    StartZeit: TDateTime;

    procedure Empfang(AntwortAnforderung: boolean;
                      VonKanal: integer; SendeNr: Int64; Nachricht: string;
                      out Antwort: string);
    procedure MemoEmpfang(Kanal: integer; Nachricht: string);
    procedure MemoSende(Von: TFileComm; Nachricht: string);

  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}             

procedure TForm1.FormCreate(Sender: TObject);

var
  num: integer;

begin
  StartZeit := Now;
  for num := 0 to High(Comm) do
  begin
    if num = 0 then
      begin
        Comm[num] := TFileComm.Create(1001, 'DMT');
        Comm[num].BeiEmpfang := Empfang;
      end
    else
      Comm[num] := TFileComm.Create('DMT');
    if Comm[num].Fehler = fcerrOk then
      case num of
        0: Label1.Caption := 'Ok ' + inttoStr(Comm[num].Kanal);
        1: Label2.Caption := 'Ok ' + inttoStr(Comm[num].Kanal);
        2: Label3.Caption := 'Ok ' + inttoStr(Comm[num].Kanal);
      end;
  end;
  for num := 0 to High(Clients) do
  begin
    Clients[num] := TFileComm.Create(Comm[0].Kanal, 'DMT');
    Clients[num].BeiEmpfang := Empfang;
  end;
  Comm[0].Name := 'Sender 1';
  Label4.Caption := InttoStr(Comm[0].EmpfaengerAnzahl(Comm[0].Kanal));
end;


procedure TForm1.FormDestroy(Sender: TObject);

var
  num: integer;
  
begin
  for num := 0 to High(Clients) do
    Clients[num].Free;
  for num := 0 to High(Comm) do
    Comm[num].Free;
end;


procedure TForm1.butSendeN1Click(Sender: TObject);

var
  Antworten: StringFeld;
  Nachricht: string;
  num: integer;

begin
  with Comm[0] do
  begin
    Inc(SendeZaehler);
    Nachricht := 'DMT ' + IntToStr(SendeZaehler);
    MemoSende(Comm[0], Nachricht);
    Antworten := SendenWarten(Nachricht, 0, 60);
    if Length(Antworten) > 0 then
      begin
        for num := 0 to High(Antworten) do
          MemoEmpfang(Kanal, Antworten[num]);
      end
    else
      MemoEmpfang(0, 'KEINE ANTWORT!');
  end;
end;


procedure TForm1.Empfang(AntwortAnforderung: boolean;
                         VonKanal: integer; SendeNr: Int64; Nachricht: string;
                         out Antwort: string);
begin
  if AntwortAnforderung then
    begin
      // Antworten auf eine eingegangene Nachricht:
      Antwort := 'NIX!!!';
      Inc(AntwortZaehler);
      if AnsiLeftStr(Nachricht, 9) = 'DMT ASYNC' then
        begin
          Antwort := 'ASNYC für "' + Trim(AnsiMidStr(Nachricht, 5, 10)) +
                     '" - S#' + IntToStr(SendeNr) +
                     ' - ' + IntToStr(AntwortZaehler);
        end
      else if AnsiLeftStr(Nachricht, 4) = 'DMT ' then
        Antwort := 'Hallo DMT!! für "' + Trim(AnsiMidStr(Nachricht, 5, 10)) +
                   '" - ' + IntToStr(AntwortZaehler)
      else if Nachricht = '' then
        ;
    end
  else
    begin
      // Empfang einer Antwort für eine mit Sende() versendete Nachricht
      MemoEmpfang(VonKanal, 'ANTWORT auf ' + IntToStr(SendeNr) + ': ' +
                            Nachricht);
    end;
end;


procedure TForm1.butLoeschenClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;


procedure TForm1.MemoSende(Von: TFileComm; Nachricht: string);
begin
  Memo1.Lines.Add(DoubleToStr(SecondSpan(Now, StartZeit), 6, 2) + ':  ' +
                  'Sende von "' + Von.Name +
                  '" auf Kanal ' + IntToStr(Von.Kanal) + ': ' +
                  Nachricht);
  SendMessage(Memo1.Handle, EM_SCROLLCARET, 0, 0);
end;


procedure TForm1.MemoEmpfang(Kanal: integer; Nachricht: string);
begin
  Nachricht := Trim(Nachricht);
  if Kanal <> 0 then
    Memo1.Lines.Add(DoubleToStr(SecondSpan(Now, StartZeit), 6, 2) + ':  ' +
                    'Empfang von Kanal ' + IntToStr(Kanal) + ': ' +
                    Nachricht)
  else
    Memo1.Lines.Add(DoubleToStr(SecondSpan(Now, StartZeit), 6, 2) + ':  ' +
                    Nachricht);
  SendMessage(Memo1.Handle, EM_SCROLLCARET, 0, 0);
end;


procedure TForm1.butSende1N10Click(Sender: TObject);

var
  num: integer;

begin
  for num := 1 to 10 do
   butSendeN1Click(nil);
end;


procedure TForm1.Button1Click(Sender: TObject);

var
  Nachricht: string;

begin
  with Comm[0] do
  begin
    Inc(SendeZaehler);
    Nachricht := 'DMT ASYNC ' + IntToStr(SendeZaehler);
    MemoSende(Comm[0], Nachricht);
    Senden(Nachricht);
  end;
end;

end.
