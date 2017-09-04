{ ==============================================================================
  DH_FileComm

  Ermöglicht die Kommunikation zwischen Programmen basierend auf
  Nachrichtendateien.
  
  Nachrichten senden mit:
  - SendenWarten (synchron)
  - Senden (asynchron)

  Empfang mit:
  - Ereignis BeiEmpfang

  Arbeitszeit:

  23.11.06    1.5h Fehler gesucht im IntelliEd
  28.11.06    2.5h Fehler behoben im IntelliEd, Funktionsliste fortgesetzt
  07.12.06    1.5h Funktionsliste abgeschlossen
  21.12.06    1.5h Ein-/Auskommentieren
  01.02.07    1.5h Ein-/Auskommentieren
  01.03.07    2.5h Ein-/Auskommentieren
  22.03.07    2 h  Ein-/Auskommentieren fertig gestellt

  12.04.07    2.5h Beginn FileComm-Projekt
  26.04.07    2.5h FileComm Steuerdatei fertig,
                   Ausarbeitung System Sende-/Empfangsdateien
  03.05.07    2.0h FileComm SendenWarten und Empfangsmechanismus
  10.05.07    2,5h FileComm Tests und Korrekturen
  31.05.07    2h   FileComm: Datei-Garbage-Collection in verschiedenen
                   Situationen (Create, Destroy, Zeitgesteuert)
  14.06.07    1.5h Asynchrones senden. V1 FERTIG!
 ============================================================================= }
unit DH_FileComm;


// ========================= Interface - Deklarationen =========================
interface


uses
  SysUtils, ExtCtrls, Classes, DateUtils, StrUtils, Forms,

  DH_Basisroutinen;


const
  // Bezeichnung der Steuerdatei, in der sich alle TFileComm-Objekte
  // eintragen und ein Hearbeat-Signal abliefern, während sie laufen:
  cSteuerDatei = 'FileComm.ini';

  // Hearbeat-Intervall in Sekunden
  cHeartBeat = 2;
  // Maximale Gültigkeitsdauer eines Heartbeats: Ist die Gültigkeitsdauer
  // überschritten, entfernen ANDERE TFileComm-Objekte die ungültigen
  // Einträge beim Abmelden automatisch aus der Steuerdatei!
  cHeartBeatUngueltig = 3 * cHeartBeat;

  // Intervall für die Prüfung/Löschung veralteter TFileComm-Dateien
  // in Sekunden:
  cGarbageIntervall = 15 * 60;

  // Maximale Gültigkeitsdauer für asynchron gesendete Nachrichten
  // in Sekunden (nach Ablauf werden die zugehörigen Nachrichtendateien
  // gelöscht):
  cAsyncGueltig = 10;

  // Kanalwert für "Broadcast - an alle"
  cCommBroadcast = -1;

  // Datei-Prefix für TFileComm-Objekte
  cFCPreFix = 'FCX_';


type
  TFCFehler = (fcerrOk,                // Letzte Aktion war ok!
               fcerrSteuerDatei,       // Kein Zugriff auf Steuerdatei
               fcerrAbmelden,          // Abmeldung war nicht möglich
               fcerrTimeOut            // TimeOut beim Warten auf Antwort
              );
              

  TFCNachricht = procedure(// =True, wenn der Eingang eine Nachricht ist,
                           // die von einem anderen TFileComm-Objekt gesendet
                           // wurde und beantwortet werden muss. Sonst ist es
                           // eine Antwort, die auf eine eigene, mit Sende
                           // versendete Nachricht eingetroffen ist.
                           AntwortAnforderung: boolean;
                           // Nachricht oder Antwort von diesem Kanal:
                           VonKanal: integer;
                           // Nachricht oder Antwort mit dieser Sendenr:
                           SendeNr: Int64;
                           // Die Nachricht
                           Nachricht: string;
                           // Nur, wenn AntwortAnforderung = True: Die
                           // Antwort auf eine eingehende Nachricht, die an
                           // den Sender zurück übermittelt werden soll:
                           out Antwort: string) of object;


  // Für das Speichern von Informationen einer mit Sende gesendeten
  // asynchronen Nachricht:
  RAsyncSendeDaten = record
    SendeNr: Int64;               // Sendungsnummer
    KanalNr: Integer;             // Kanalnummer
    AntwTimeOut: TDateTime;       // Maximaler Gültigkeitstermin
  end;

  TFileComm = class(TObject)
    private
      // Der im Create-Ereignis angegebene Kanal
      CommKanal: integer;
      // Pfad für die Kommunikationsdateien
      CommPfad: string;
      // Das über die Eigenschaft HeartBeatInter festgelegte HeartBeat-
      // Intervall:
      HBIntervall: integer;
      // Der Timer für das regelmäßige setzen des 'Lebenszeichens'
      HeartBeat: TTimer;
      // Der Timer für das regelmäßige Prüfen auf Nachrichteneingang
      EmpfangTimer: TTimer;
      // Garbage-Timer für das Aufäumen veraltetet TFileComm-Dateien
      GarbageTimer: TTimer;
      // Letzter Fehlercode nach einer Aktion
      LetzterFehler: TFCFehler;
      // Pfad der Steuerdatei mit den Anmeldedaten jedes TFileComm-Objekts
      // und den Lebenszeichen:
      SteuerDatei: string;
      // Objektnummer in der Steuerdatei
      CommID: double;
      // Objektnummer als String ohne Komma:
      CommIDStr: string;
      // Die fortlaufend vergebene Nummer für Sendungen
      SendeNummer: Int64;
      // Liste der noch offenen asynchronen Nachrichten:
      AsyncListe: array of RAsyncSendeDaten;

      procedure Abmelden;
      function  AntwortLesen(AusDatei: string): string;
      procedure AntwortSchreiben(Pfad: string; Antwort: string);
      procedure Aufraeumen(Zeilen: TStringList);
      function  Ausliefern(Senden: string; AnKanal: integer): Int64;
      procedure CreateInit;
      function  EingangLesen(AusDatei: string): string;
      function  EInterLesen: integer;
      procedure EInterSchreiben(Intervall: integer);
      procedure EmpfangTimerTick(Sender: TObject);
      procedure GarbageTimerTick(Sender: TObject);
      procedure HBISchreiben(IntervallSek: integer);
      procedure HeartBeatMelden(FS: TFileStream = nil;
                                Zeilen: TStringList = nil;
                                num: integer = 0);
      procedure HeartBeatStarten;
      procedure HeartBeatTimerTick(Sender: TObject);
      function  KanalPfad(AnKanal: integer; Sendung: Int64): string;
      procedure SteuerDateiPfad(AppDir: string; CommDir: string = '');
      procedure SteuerZeile(out FS: TFileStream;
                            out Zeilen: TStringList;
                            out CommZeile: integer);
      function  SucheEmpfang(AnKanal: integer; Sendung: Int64): StringFeld;

    public
      // Bezeichnung des Objekts (wahlweise)
      Name: string;

      // Standard-Wartezeit in Sekunden
      StandardTimeOut: double;

      // Das Ereignis für den Empfang von Nachrichten
      BeiEmpfang: TFCNachricht;

      // Intervall für Empfangsabfrage-Intervall in Sekunden
      property EmpfangInter: integer read EInterLesen write EInterSchreiben;
      property Fehler: TFCFehler read LetzterFehler;
      // Intervall für Lebenszeichen-Signal in Sekunden
      property HeartBeatInter: integer read HBIntervall write HBISchreiben;
      // Liefert die verwendete Kanalnummer
      property Kanal: integer read CommKanal;

      procedure FehlerLoeschen;
      function  EmpfaengerAnzahl(Kanal: integer = 0): integer;
      function  Senden(SendeText: string; AnKanal: integer = 0): Int64;
      function  SendenWarten(Senden: string; AnKanal: integer = 0;
                             TimeOut: double = 0): StringFeld;

      constructor Create(Kanal: integer;
                         AppDir: string; CommDir: string = ''); overload;
      constructor Create(AppDir: string; CommDir: string = ''); overload;
      destructor Destroy; override;
  end;


implementation


// TFileComm -------------------------------------------------------------------


{ ------------------------------------------------------------------------------
  TFileComm.Create

  Anlegen des Objekts mit Anmeldung in der zentralen Steuerdatei für
  TFileComm-Objekte.

  Parameter, Version 1:
  Kanal       Kommunikations-Kanalnummer, die vom Objekt verwendet werden
              soll.
  AppDir      Unterverzeichnis im Anwendungsverzeichnis, das die
              Kommunikationsdateien enthält (wird nötigenfalls angelegt).
  CommDir     Keine Angabe: Es wird das Standard-Anwendungsverzeichnis für
              den aktuellen Windows-Anwender verwendet. Sonst das zu
              verwendende Verzeichnis angeben.

  Parameter, Version 2:
  AppDir      Unterverzeichnis im Anwendungsverzeichnis, das die
              Kommunikationsdateien enthält (wird nötigenfalls angelegt).
  CommDir     Keine Angabe: Es wird das Standard-Anwendungsverzeichnis für
              den aktuellen Windows-Anwender verwendet. Sonst das zu
              verwendende Verzeichnis angeben.
  Bei Version 2 wird die nächste freie Kanalnummer automatisch ermittelt und
  dem Objekt zugeordnet.
  ---------------------------------------------------------------------------- }
constructor TFileComm.Create(Kanal: integer;
                             AppDir: string; CommDir: string = '');
begin
  CreateInit;

  if Kanal = 0 then Kanal := 1000;
  CommKanal := Kanal;

  SteuerDateiPfad(AppDir, CommDir);
  HeartBeatMelden;

  HeartBeatStarten;
end;


constructor TFileComm.Create(AppDir: string; CommDir: string = ''); 

var
  FreiKanal, ZKanal: integer;
  CommZeile: integer;
  Token: TStringList;
  FS: TFileStream;
  Zeilen: TStringList;
  num: integer;

begin
  CreateInit;
  SteuerDateiPfad(AppDir, CommDir);
  if DateiOeffnen(SteuerDatei, tdoExklusiv or tdoWarten, FS, 5) = tdoExklusiv then
    begin
      Zeilen := TStringList.Create;
      Zeilen.LoadFromStream(FS);

      Aufraeumen(Zeilen);
      
      // Suche eine freie Kanalnummer
      FreiKanal := 1000;
      repeat
        Inc(FreiKanal);
        // Kanalnummer schon vorhanden?
        CommZeile := 0;
        while CommZeile < Zeilen.Count do
        begin
          Token := StrSplit(Zeilen[CommZeile], ';');
          if Token.Count >= 1 then
          begin
            ZKanal := StrToInteger(Token[0]);
            if ZKanal = FreiKanal then break;
          end;
          Inc(CommZeile);
        end;
        // Nein? Dann fertig!
      until CommZeile = Zeilen.Count;

      // Freie Zeile in der Steuerdatei vorgeben
      CommKanal := FreiKanal;
      Zeilen.Add('');
      num := Zeilen.Count - 1;
      // und Heartbeat schreiben:
      HeartBeatMelden(FS, Zeilen, num);

      HeartBeatStarten;
    end
  else
    LetzterFehler := fcerrSteuerDatei;
end;


destructor TFileComm.Destroy;
begin
  HeartBeat.Free;
  Abmelden;
  inherited;
end;


{ ------------------------------------------------------------------------------
  TFileComm.Abmelden

  Trägt das Objekt aus der Steuerdatei aus.
  ---------------------------------------------------------------------------- }
procedure TFileComm.Abmelden;

var
  FS: TFileStream;
  Zeilen: TStringList;
  num: integer;

begin
  SteuerZeile(FS, Zeilen, num);
  if Assigned(FS) then
    begin
      FS.Size := 0;
      Zeilen.Delete(num);

      Aufraeumen(Zeilen);

      // Übrig gebliebene, nicht mehr bearbeitete Dateien dieses Objekts
      // löschen:
      EraseFiles(CommPfad + cFCPreFix + IntToStr(CommKanal) + '_' + CommIDStr + '_*.*');

      // Auch Quittungen dieses Objekts löschen, die von den Nachrichtensendern
      // nicht gelöscht wurden:
      EraseFiles(CommPfad + cFCPreFix + '*.Q' + CommIDStr);

      Zeilen.SaveToStream(FS);
      Zeilen.Free;
      FS.Free;
    end
  else
    LetzterFehler := fcerrAbmelden;
end;


{ ------------------------------------------------------------------------------
  TFileComm.AntwortLesen

  Liest eine Antwort aus einer Antwortdatei und löscht sie anschließend.

  Parameter:
  AusDatei    Pfad- und Dateiname der Antwortdatei.

  Ergebnis:
  Die Antwort (='', wenn kein Zugriff möglich oder die Datei leer war).
  ---------------------------------------------------------------------------- }
function TFileComm.AntwortLesen(AusDatei: string): string;

var
  FS: TFileStream;
  Zeilen: TStringList;

begin
  Result := '';
  if DateiOeffnen(AusDatei, tdoExklusiv or tdoWarten, FS, 5) = tdoExklusiv then
  begin

    Zeilen := TStringList.Create;
    Zeilen.LoadFromStream(FS);
    Result := Zeilen.Text;
    Zeilen.Free;

    FS.Free;

    EraseFiles(AusDatei);
  end;
end;


{ ------------------------------------------------------------------------------
  TFileComm.AntwortSchreiben

  Erstellt die Datei zum Beantworten einer Nachricht.

  Parameter:
  Pfad        Pfad- und Dateiname der Antwortdatei.
  Antwort     Der zu Antwort-Text.
  ---------------------------------------------------------------------------- }
procedure TFileComm.AntwortSchreiben(Pfad: string; Antwort: string);

var
  Datei: TStringList;

begin
  Datei := TStringList.Create;
  Datei.Add(Antwort);
  Datei.SaveToFile(Pfad);
  Datei.Free;
end;


{ ------------------------------------------------------------------------------
  TFileComm.Aufraeumen

  Entfernt aus einer Liste der Zeilen aus der Steuerdatei die ungültigen
  und veralteten Einträge.

  Parameter:
  Zeilen      Die Zeilen aus der Steuerdatei.
  ---------------------------------------------------------------------------- }
procedure TFileComm.Aufraeumen(Zeilen: TStringList);

var
  Token: TStringList;
  num: integer;
  
begin
  num := 0;
  while num < Zeilen.Count do
  begin
    Token := StrSplit(Zeilen[num], ';');
    if Token.Count < 3 then
      // Ungültiger Steuereintrag: Entfernen!
      Zeilen.Delete(num)
    else if abs(SecondSpan(Now, StrToDateTime(Token[2]))) >
            cHeartBeatUngueltig then
      begin
        // Veralteter Steuereintrag: Entfernen!
        Zeilen.Delete(num);
        // Auch alle zugehörigen Dateien löschen, die evtl. noch
        // zum veralteten Eintrag vorhanden sind:
        if (Token[0] <> '') and (Token[1] <> '') then
        begin
          Token[1] := AnsiReplaceStr(Token[1], ',', '');
          Token[1] := AnsiReplaceStr(Token[1], '.', '');
          EraseFiles(CommPfad + cFCPreFix + Token[0] + '_' + Token[1] + '_' + '*.*');
        end;
      end
    else
      Inc(num);
  end;
end;


{ ------------------------------------------------------------------------------
  TFileComm.Ausliefern

  Erstellt die Datei zum Senden einer Nachricht.

  Parameter:
  Senden      Der zu sendende Text.
  AnKanal     Zielkanal für den zu sendenden Text (=0: Sendung erfolgt an
              den Kanal, der dem TFileComm-Objekt zugewiesen wurde)

  Ergebnis:
  Die Sendenummer der Datei.
  ---------------------------------------------------------------------------- }
function TFileComm.Ausliefern(Senden: string; AnKanal: integer): Int64;

var
  Datei: TStringList;

begin
  Datei := TStringList.Create;
  Datei.Add(Senden);
  Inc(SendeNummer);
  Datei.SaveToFile(KanalPfad(AnKanal, SendeNummer) + '.snd');
  Result := SendeNummer;
  Datei.Free;
end;


{ ------------------------------------------------------------------------------
  TFileComm.CreateInit

  Führt einheitlich für beide Creates die einleitenden Aktionen aus.
  ---------------------------------------------------------------------------- }
procedure TFileComm.CreateInit;
begin
  LetzterFehler := fcerrOk;
  CommID := GenUniqueAppID;
  CommIDStr := DoubleToStr(CommID, 15, 14, True);
  CommIDStr := AnsiReplaceStr(CommIDStr, ',', '');
  CommIDStr := AnsiReplaceStr(CommIDStr, '.', '');
  StandardTimeOut := 10;
  EmpfangTimer := TTimer.Create(nil);
  EmpfangTimer.OnTimer := EmpfangTimerTick;
  EmpfangInter := 500;
  GarbageTimer := TTimer.Create(nil);
  GarbageTimer.OnTimer := GarbageTimerTick;
  GarbageTimer.Interval := cGarbageIntervall * 1000;
  GarbageTimer.Enabled := True;
end;


{ ------------------------------------------------------------------------------
  TFileComm.EingangLesen

  Liest eine Anfrage aus einer Sendedatei.

  Parameter:
  AusDatei    Pfad- und Dateiname der Sendedatei.

  Ergebnis:
  Der gesendete Inhalt (='', wenn kein Zugriff möglich oder Datei leer war).
  ---------------------------------------------------------------------------- }
function TFileComm.EingangLesen(AusDatei: string): string;

var
  FS: TFileStream;
  Zeilen: TStringList;

begin
  Result := '';
  if DateiOeffnen(AusDatei, tdoLesen or tdoWarten, FS, 5) <> 0 then
  begin

    Zeilen := TStringList.Create;
    Zeilen.LoadFromStream(FS);
    Result := Zeilen.Text;
    Zeilen.Free;

    FS.Free;
  end;
end;


{ ------------------------------------------------------------------------------
  TFileComm.EInterLesen
  TFileComm.EInterSchreiben

  Lese-/Schreib-Teil der EmpfangInter-Eigenschaft.

  Lesen oder Festlegen des Prüfintervalls für den Nachrichteneingang.

  Parameter:
  Intervall   Intervall in Millisekunden.

  Ergebnis:
  Aktuelles Intervall.
  ---------------------------------------------------------------------------- }
function TFileComm.EInterLesen: integer;
begin
  Result := EmpfangTimer.Interval;
end;

procedure TFileComm.EInterSchreiben(Intervall: integer);
begin
  EmpfangTimer.Interval := Intervall;
  EmpfangTimer.Enabled := False;
  EmpfangTimer.Enabled := True;
end;


{ ------------------------------------------------------------------------------
  TFileComm.EmpfaengerAnzahl

  Liefert die Anzahl der verfügbaren TFileComm-Empfänger für einen
  bestimmten Kanal.

  Parameter:
  Kanal       Kanalnummer, für den die Empfängeranzahl geliefert werden soll.
              (=0, Anzahl aller Empfänger melden).

  Ergebnis:
  Die Anzahl (wenn dieses Objekt die angegebene Kanalnummer hat, zählt es
  bei der Anzahl mit!).
  ---------------------------------------------------------------------------- }
function TFileComm.EmpfaengerAnzahl(Kanal: integer = 0): integer;

var
  FS: TFileStream;
  Zeilen: TStringList;
  Token: TStringList;
  num: integer;

begin
  Result := 0;
  LetzterFehler := fcerrOk;
  if DateiOeffnen(SteuerDatei, tdoLesen or tdoWarten, FS, 5) <> 0 then
    begin
      Zeilen := TStringList.Create;
      Zeilen.LoadFromStream(FS);

      // Suche alle angemeldeten TFileComm-Objekte, die zu der
      // angegebenen Kanalnummer passen:
      for num := 0 to Zeilen.Count - 1 do
      begin
        Token := StrSplit(Zeilen[num], ';');
        if Token.Count >= 3 then
          if (StrToInteger(Token[0]) = Kanal) or (Kanal = 0) then
          begin
            // Nur Empfänger mit gültigem Heartbeat zählen!
            if abs(SecondSpan(Now, StrToDateTime(Token[2]))) <=
               cHeartBeatUngueltig then Inc(Result);
          end;
        Token.Free;
      end;

      FS.Free;
    end
  else
    LetzterFehler := fcerrSteuerDatei;
end;


{ ------------------------------------------------------------------------------
  TFileComm.EmpfangTimerTick

  Empfängt das EmpfangTimer.OnTimer-Ereignis und prüft, ob ein Nachrichten-
  Eingang vorliegt.
  ---------------------------------------------------------------------------- }
procedure TFileComm.EmpfangTimerTick(Sender: TObject);

var
  Quittung: TStringList;
  Eigene, QuittungsDatei, AntwortDatei: string;
  Eingang: string;
  Antwort: string;
  Antworten: StringFeld;
  Dummy: string;
  SendeNr: integer;
  SR: TSearchRec;
  num, i: integer;

begin

  // -----------------------------------------------------
  // Gibt es Anforderungen, die beantwortet werden müssen?
  // -----------------------------------------------------

  if FindFirst(CommPfad + cFCPreFix + IntToStr(CommKanal) + '_*.snd', 0, SR) = 0 then
  begin
    repeat
      // Eigene Sendungen nicht bearbeiten!
      Eigene := cFCPreFix + IntToStr(CommKanal) + '_' +  CommIDStr + '_';
      if AnsiLeftStr(SR.Name, Length(Eigene)) <> Eigene then
      begin
        // Sendung schon bearbeitet?
        QuittungsDatei := CommPfad + AnsiLeftStr(SR.Name, Length(SR.Name) - 3) +
                          'Q' + CommIDStr;
        if not FileExists(QuittungsDatei) then
        begin

          // Sendenummer aus dem Dateinamen ermitteln:
          num := PosRev('_', SR.Name);
          SendeNr := StrToInt64(AnsiMidStr(SR.Name, num + 1,
                                Length(SR.Name) - num - 4));

          // Nachrichteninhalt lesen
          Eingang := EingangLesen(CommPfad + SR.Name);

          // Quittungsdatei schreiben
          Quittung := TStringList.Create;
          Quittung.Add('Q');
          Quittung.SaveToFile(QuittungsDatei);
          Quittung.Free;

          // Ereignis feuern
          if (Eingang <> '') and Assigned(BeiEmpfang) then
          begin
            BeiEmpfang(True, CommKanal, SendeNr, Eingang, Antwort);

            // Antwort schreiben
            if Antwort <> '' then
            begin
              AntwortDatei := AnsiLeftStr(SR.Name, Length(SR.Name) - 3) +
                              CommIDStr;
              AntwortSchreiben(CommPfad + AntwortDatei, Antwort);
            end;

          end;
        end;
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;

  // ------------------------------------------------------
  // Gibt es Antworten auf mit Sende gesendete Nachrichten?
  // ------------------------------------------------------

  // Überhaupt was asynchron gesendet?
  if (Length(AsyncListe) > 0) and Assigned(BeiEmpfang) then
    for num := 0 to High(AsyncListe) do
      with AsyncListe[num] do
      begin
        // Gibts Antworten?
        Antworten := SucheEmpfang(KanalNr, SendeNr);
        // Antwortdateien auswerten:
        for i := 0 to High(Antworten) do
        begin
          Antwort := AntwortLesen(Antworten[i]);
          if Antwort <> '' then
            BeiEmpfang(False, KanalNr, SendeNr, Antwort, Dummy);
        end;
      end;
end;


{ ------------------------------------------------------------------------------
  TFileComm.FehlerLoeschen

  Quittiert die letzte Fehlermeldung und setzt den Fehlerstatus zurück.
  ---------------------------------------------------------------------------- }
procedure TFileComm.FehlerLoeschen;
begin
  LetzterFehler := fcerrOk;
end;


{ ------------------------------------------------------------------------------
  TFileComm.GarbageTimerTick

  Empfängt OnTick von GarbageTimer.

  Löschen aller veralteter TFileComm-Dateien.
  ---------------------------------------------------------------------------- }
procedure TFileComm.GarbageTimerTick(Sender: TObject);

var
  SR: TSearchRec;
  Dateien: TStringList;
  Stempel: integer;
  num: integer;

begin
  if CommPfad = '' then Exit;

  // Suche alle aktuell vorhandenen TFileComm-Dateien:
  Dateien := TStringList.Create;
  if FindFirst(CommPfad + cFCPreFix + '*.*', 0, SR) = 0 then
  begin
    repeat
      Dateien.Add(SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;

  // Lösche alle veralteten Dateien
  for num := 0 to Dateien.Count - 1 do
  begin
    Stempel := FileAge(CommPfad + Dateien[num]);
    if Stempel <> -1 then
      if SecondSpan(Now, FileDateToDateTime(Stempel)) > cGarbageIntervall then
        EraseFiles(CommPfad + Dateien[num]);
  end;

  Dateien.Free;
end;


{ ------------------------------------------------------------------------------
  TFileComm.HBISchreiben

  Write-Teil der HeartBeatInter-Eigenschaft.

  Parameter:
  IntervallSek     Das neue Heartbeat-Intervall in Sekunden.
  ---------------------------------------------------------------------------- }
procedure TFileComm.HBISchreiben(IntervallSek: integer);
begin
  HBIntervall := IntervallSek;
  HeartBeat.Interval := HBIntervall * 1000;
  HeartBeat.Enabled := False;
  HeartBeat.Enabled := True;
end;


{ ------------------------------------------------------------------------------
  TFileComm.HeartBeatMelden

  Schreibt für das Objekt einen aktuellen Heartbeat-Eintrag in die
  Steuerdatei.
  ---------------------------------------------------------------------------- }
procedure TFileComm.HeartBeatMelden(FS: TFileStream = nil;
                                    Zeilen: TStringList = nil;
                                    num: integer = 0);
begin
  if not Assigned(FS) then SteuerZeile(FS, Zeilen, num);
  if Assigned(FS) then
  begin

    Zeilen[num] := IntToStr(CommKanal) + ';' +
                   DoubleToStr(CommID, 15, 11, True) + ';' +
                   DateTimeToStr(Now);

    FS.Size := 0;
    Zeilen.SaveToStream(FS);

    Zeilen.Free;
    FS.Free;
  end;
end;


{ ------------------------------------------------------------------------------
  TFileComm.HeartBeatStarten

  Heartbeat-Timer anlegen und mit Standardintervall starten.
  ---------------------------------------------------------------------------- }
procedure TFileComm.HeartBeatStarten;
begin
  HeartBeat := TTimer.Create(nil);
  HeartBeat.OnTimer := HeartBeatTimerTick;
  HeartBeatInter := cHeartBeat;
end;


{ ------------------------------------------------------------------------------
  TFileComm.HeartBeatTimerTick

  Empfängt HeartBeatTimer.OnTick: Heartbeat-Eintrag in der Steuerdatei
  aktualisieren und abgelaufene Dateien für asynchrone Nachrichten löschen.
  ---------------------------------------------------------------------------- }
procedure TFileComm.HeartBeatTimerTick(Sender: TObject);

var
  num, i: integer;

begin
  HeartBeatMelden;

  // Löschen abgelaufener asynchroner Nachrichtendateien
  num := 0;
  while num <= High(AsyncListe) do
    with AsyncListe[num] do
      if Now > AntwTimeOut then
        begin
          EraseFiles(KanalPfad(KanalNr, SendeNr) + '.*');
          for i := num to High(AsyncListe) - 1 do
            AsyncListe[i] := AsyncListe[i + 1];
          SetLength(AsyncListe, Length(AsyncListe) - 1);
        end
      else
        Inc(num);
end;


{ ------------------------------------------------------------------------------
  TFileComm.KanalPfad

  Liefert den Pfad- und Dateinamen für eine Sende- oder Antwortdatei (ohne
  Dateinamenerweiterung).

  Parameter:
  AnKanal     Zielkanal für den zu sendenden Text (=0: Sendung erfolgt an
              den Kanal, der dem TFileComm-Objekt zugewiesen wurde).
  Sendung     Nummer der Sendung.

  Ergebnis:
  Pfad- und Dateiname.
  ---------------------------------------------------------------------------- }
function TFileComm.KanalPfad(AnKanal: integer; Sendung: Int64): string;
begin
  Result := CommPfad + cFCPreFix + IntToStr(AnKanal) + '_' +  CommIDStr + '_' +
                       IntToStr(Sendung);

end;


{ ------------------------------------------------------------------------------
  TFileComm.Senden

  Sendet einen Nachrichtentext ohne warten auf Antwort.

  Antworten über das BeiEmpfang-Ereignis.

  Parameter:
  Senden      Der zu sendende Text.
  AnKanal     Zielkanal für den zu sendenden Text (=0: Sendung erfolgt an
              den Kanal, der dem TFileComm-Objekt zugewiesen wurde)

  Ergebnis:
  Sendungsnummer für die Nachricht.
  ---------------------------------------------------------------------------- }
function TFileComm.Senden(SendeText: string; AnKanal: integer): Int64;
begin
  // Standardeinstellung, wenn nicht anders angegeben:
  if AnKanal = 0 then AnKanal := Kanal;
  Result := Ausliefern(SendeText, AnKanal);
  // Sendung zur Liste der zu überwachenden Async-Dateien ergänzen:
  SetLength(AsyncListe, Length(AsyncListe) + 1);
  with AsyncListe[High(AsyncListe)] do
  begin
    SendeNr := Result;
    KanalNr := AnKanal;
    AntwTimeOut := IncSecond(Now, cAsyncGueltig);
  end;
end;


{ ------------------------------------------------------------------------------
  TFileComm.SendenWarten

  Sendet eine Nachricht und wartet auf die Antwort von allen Empfängern.

  Parameter:
  Senden      Der zu sendende Text.
  AnKanal     Zielkanal für den zu sendenden Text (=0: Sendung erfolgt an
              den Kanal, der dem TFileComm-Objekt zugewiesen wurde)
  TimeOut     TimeOut-Zeit für das Warten auf alle Antworten in Sekunden,
              falls nicht der Standard-Timeout verwendet werden soll.

  Ergebnis:
  Die innerhalb des TimeOut-Zeiffensters empfangenen Antworten.
  ---------------------------------------------------------------------------- }
function TFileComm.SendenWarten(Senden: string; AnKanal: integer = 0;
                                TimeOut: double = 0): StringFeld;
var
  StartZeit: TDateTime;
  Antworten: integer;
  Antwort: string;
  Empfange: StringFeld;
  GesendetNr: Int64;
  num: integer;

begin
  Result := nil;

  // Standardeinstellung, wenn nicht anders angegeben:
  if AnKanal = 0 then AnKanal := Kanal;
  if TimeOut <= 0 then TimeOut := StandardTimeOut;

  // Soviele Antworten werden erwartet:
  Antworten := EmpfaengerAnzahl(AnKanal) - 1;
  if Antworten < 1 then Exit;

  // Daten schreiben:
  GesendetNr := Ausliefern(Senden, AnKanal);

  // Antwortanzahl oder TimeOut abwarten:
  StartZeit := Now;
  while (Length(Result) < Antworten) and
        (SecondSpan(Now, StartZeit) <= TimeOut) do
  begin
    // Gibts Antworten?
    Empfange := SucheEmpfang(AnKanal, GesendetNr);
    // Antwortdateien auswerten:
    for num := 0 to High(Empfange) do
    begin
      Antwort := AntwortLesen(Empfange[num]);
      if Antwort <> '' then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := Antwort;
      end;
    end;
    // Den anderen Prozessen eine Chance geben
    Application.ProcessMessages;
  end;

  // Sende-/Quittungs- und Antwortdateien löschen
  EraseFiles(KanalPfad(AnKanal, GesendetNr) + '.*');

  if Length(Result) < Antworten then LetzterFehler := fcerrTimeOut;
end;


{ ------------------------------------------------------------------------------
  TFileComm.SteuerDateiPfad

  Pfad der Steuerdatei ermitteln und nötigenfalls anlegen.

  Parameter:
  AppDir      Unterverzeichnis im Anwendungsverzeichnis, das die
              Kommunikationsdateien enthält (wird nötigenfalls angelegt).
  CommDir     Keine Angabe: Es wird das Standard-Anwendungsverzeichnis für
              den aktuellen Windows-Anwender verwendet. Sonst das zu
              verwendende Verzeichnis angeben.
  ---------------------------------------------------------------------------- }
procedure TFileComm.SteuerDateiPfad(AppDir: string; CommDir: string);
begin
  // Anlegen der Datei im Anwendungsdaten-Verzeichnis des Anwenders,
  // wenn nicht anders angegeben:
  if CommDir = '' then CommDir := GetShellFolder(CSIDL_APPDATA);
  CommPfad := PfadAngabeVollstaendig(PfadAngabeVollstaendig(CommDir) + AppDir);
  ForceDirectories(CommPfad);
  SteuerDatei := CommPfad + cSteuerDatei;
end;


{ ------------------------------------------------------------------------------
  TFileComm.SteuerZeile

  Liefert Zugriff auf die Steuerdatei. Die Steuerdatei enthält für jedes
  TFileComm-Objekt einen Steuereintrag (zeilenweise) in folgendem Format:

  Kanalnummer;Objekt-ID;Heartbeat (Datum Uhrzeit)

  Ergebnis:
  FS          Filestream auf die Steuerdatei
  Zeilen      Inhalt der Steuerdatei beim Öffnen
  num         Zeilennummer innerhalb der Steuerdatei (ab 0), die den
              Eintrag für dieses Objekt enthält.
  ---------------------------------------------------------------------------- }
procedure TFileComm.SteuerZeile(out FS: TFileStream;
                                out Zeilen: TStringList;
                                out CommZeile: integer);
var
  Token: TStringList;
  ZKanal: integer;

begin
  if DateiOeffnen(SteuerDatei, tdoExklusiv or tdoWarten, FS, 5) = tdoExklusiv then
    begin
      Zeilen := TStringList.Create;
      Zeilen.LoadFromStream(FS);

      CommZeile := 0;
      while CommZeile < Zeilen.Count do
      begin
        Token := StrSplit(Zeilen[CommZeile], ';');
        if Token.Count >= 3 then
        begin
          ZKanal := StrToInteger(Token[0]);
          if ZKanal = CommKanal then
            if SameDouble(CommID, StrToDouble(Token[1])) then break;
        end;
        Token.Free;
        Inc(CommZeile);
      end;
      if CommZeile = Zeilen.Count then Zeilen.Add('');

    end
  else
    begin
      FS := nil;
      Zeilen := nil;
      CommZeile := -1;
      LetzterFehler := fcerrSteuerDatei;
    end;
end;


{ ------------------------------------------------------------------------------
  TFileComm.SucheEmpfang

  Sucht Empfangs(Antwort)-Dateien, die dem angegebenen Muster entsprechen.

  Parameter:
  AnKanal     Nummer des Kanal, von dem Antworten erwartet werden.
  Sendung     Nummer der Sendung, für die Antworten erwartet werden.

  Ergebnis:
  Pfade und Namen der gefundenen Dateien.
  ---------------------------------------------------------------------------- }
function TFileComm.SucheEmpfang(AnKanal: integer; Sendung: Int64): StringFeld;

var
  Ext: string;
  SR: TSearchRec;

begin
  Result := nil;
  if FindFirst(KanalPfad(AnKanal, Sendung) + '.*', 0, SR) = 0 then
  begin
    repeat
      Ext := AnsiLowerCase(ExtractFileExt(SR.Name));
      if (Ext <> '.snd') and (LeftBStr(Ext, 2) <> '.q') then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := CommPfad + SR.Name;
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

end.



