
{***********************************************}
{*    Connect 4 - Player Related Attributes    *}
{* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *}
{*                                             *}
{*    Author: Nathan Bartram                   *}
{*    Contact: nathan_bartram@hotmail.co.uk    *}
{*    Licence: GPL                             *}
{*                                             *}
{***********************************************}

unit PlayersDialog;

{$mode objfpc}{$H+}

interface

uses
  LResources, Forms, Controls, Graphics, ButtonPanel, StdCtrls, ExtCtrls, Classes;

type

  { TPlayersDialogForm }

  TPlayersDialogForm = class(TForm)
    ButtonPanel: TButtonPanel;
    Player1TypeCPURadioButton: TRadioButton;
    Player1TypeHumRadioButton: TRadioButton;
    Player2LabeledEdit: TLabeledEdit;
    Player2Coin: TImage;
    Player1GroupBox: TGroupBox;
    Player1Coin: TImage;
    Player2GroupBox: TGroupBox;
    Player1LabeledEdit: TLabeledEdit;
    Player2TypeCPURadioButton: TRadioButton;
    Player2TypeHumRadioButton: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure PlayerRadioButtonClick(Sender: TObject);
  public
    IsPlayer1Bot: boolean;
    IsPlayer2Bot: boolean;
    FirstShow: boolean;
    Skin: string;
    procedure UpdateForm;
  end; 

var
  PlayersDialogForm: TPlayersDialogForm;

implementation

{ TPlayersDialogForm }
//----------------------------------------------------------------------------//
procedure TPlayersDialogForm.PlayerRadioButtonClick(Sender: TObject);
begin
  if FirstShow then // laz bug, click event triggered on first show of form
    begin           // so added varaible to check
      if Player1TypeHumRadioButton.Checked then IsPlayer1Bot:=false
      else IsPlayer1Bot:=true;
      if Player2TypeHumRadioButton.Checked then IsPlayer2Bot:=false
      else IsPlayer2Bot:=true;
    end
  else FirstShow:=true;
end;
//----------------------------------------------------------------------------//
procedure TPlayersDialogForm.FormCreate(Sender: TObject);
begin
  FirstShow:=false;
end;
//----------------------------------------------------------------------------//
procedure TPlayersDialogForm.UpdateForm;
begin
  if IsPlayer1Bot then Player1TypeCPURadioButton.Checked:=true
  else Player1TypeHumRadioButton.Checked:=true;
  if IsPlayer2Bot then Player2TypeCPURadioButton.Checked:=true
  else Player2TypeHumRadioButton.Checked:=true;
  Player1Coin.Picture.LoadFromLazarusResource(Skin+'RedCoin');
  Player2Coin.Picture.LoadFromLazarusResource(Skin+'YellowCoin');
end;
//----------------------------------------------------------------------------//

initialization
  {$I playersdialog.lrs}

end.

