program Connect4;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources, Main, PlayersDialog;

{$IFDEF WINDOWS}{$R Connect4.rc}{$ENDIF}

begin
  {$I Connect4.lrs}
  {$I CartoonSkin.res}  // Skins included here
  {$I CrystalSkin.res}  //
  Application.Initialize;
  Application.CreateForm(TPlayGround, PlayGround);
  Application.CreateForm(TPlayersDialogForm, PlayersDialogForm);
  Application.Run;
end.
