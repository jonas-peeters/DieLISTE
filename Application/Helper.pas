unit Helper;

interface

uses
  FMX.Dialogs, JSON;

implementation

{$R *.dfm}

function interpretServerResponse(response: String): Boolean;
var
  jsonObject: TJSONObject;
  errorCode: Integer;
  errorMessage: String;
begin
  try
    jsonObject := TJSONObject.ParseJSONValue(response);
    errorCode := jsonObject.GetValue('code', 0);
    case errorCode of
      10: begin result := True; break end;
      11: begin result := True; break end;
      12: begin result := True; break end;
      20: begin result := false; break end;
      21: begin result := false; break end;
      22: begin result := false; break end;
      23: begin result := false; break end;
      24: begin result := false; break end;
      25: begin result := false; ShowMessage('Unerwarteter Fehler. Sind alle eingaben korrekt?'); break end;
      30: begin result := false; break end;
      31: begin result := false; break end;
      32: begin result := false; ShowMessage('Server Fehler: Die E-Mail ist möglicherweise nicht angekommen.'); break end;
      40: begin result := false; ShowMessage('Anmeldung Fehlgeschlagen: Versuche es erneut.'); break end;
      41: begin result := false; ShowMessage('Fehler: Der User konnte nicht gefunden werden.'); break end;
      42: begin result := false; ShowMessage('Fehler: Die E-Mail konnte nicht gefunden werden.'); break end;
      43: begin result := false; ShowMessage('Anmeldung Fehlgeschlagen: Passwort oder E-Mail falsch.'); break end;
      44: begin result := false; ShowMessage('Benutzername oder E-Mail nicht verfügbar.'); break end;
      45: begin result := false; ShowMessage('Diese Aktion ist nicht möglich. Deine E-Mail ist noch nicht verifiziert.'); break end;
    end;
  except
    result := false;
  end;
end;



end.
