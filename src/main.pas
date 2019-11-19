
{***********************************************}
{*          Connect 4 - Main Form Unit         *}
{* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *}
{*                                             *}
{*    Author: Nathan Bartram                   *}
{*    Contact: nathan_bartram@hotmail.co.uk    *}
{*    Licence: GPL                             *}
{*                                             *}
{***********************************************}

unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, Buttons, ComCtrls, ImgList, Game, PlayersDialog;

const
  PaintBoxSectorsX: array[1..8] of integer = (34,104,174,244,314,384,454,518);
  PaintBoxSectorsY: array[1..7] of integer = (39,109,179,249,319,389,459);
  Down: boolean = true;
  Up: boolean = false;

type
  { TPlayGround }
  TPlayGround = class(TForm)
    CoinPreview: TImage;
    ToolBarImages: TCustomImageList;
    LoadDialog: TOpenDialog;
    MenuItemBotLevel4: TMenuItem;
    MenuItemBotLevel5: TMenuItem;
    MenuItemSkinCrystal: TMenuItem;
    MenuItemSkinCartoon: TMenuItem;
    MenuItemSelectSkin: TMenuItem;
    MenuItemPlayers: TMenuItem;
    MenuItemBotLevel1: TMenuItem;
    MenuItemBotLevel2: TMenuItem;
    MenuItemBotLevel3: TMenuItem;
    MenuItemBotSkill: TMenuItem;
    MenuItemRedo: TMenuItem;
    MenuItemUndo: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemShowStatusBar: TMenuItem;
    MenuItemNewCoin0: TMenuItem;
    MenuItemNewCoin2: TMenuItem;
    MenuItemNewCoin3: TMenuItem;
    MenuItemNewCoin1: TMenuItem;
    MenuItemNewCoinPosition: TMenuItem;
    MenuItemStart2: TMenuItem;
    MenuItemStart1: TMenuItem;
    MenuItemStart0: TMenuItem;
    MenuItemStartingPlayer: TMenuItem;
    MenuItemShowCoinPreview: TMenuItem;
    MenuItemSettings: TMenuItem;
    MenuItemShowToolBar: TMenuItem;
    MenuItemView: TMenuItem;
    SaveDialog: TSaveDialog;
    SmoothX: TTimer;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    ToolButtonLoadGame: TToolButton;
    ToolButtonSaveGame: TToolButton;
    ToolButtonDivider2: TToolButton;
    ToolButtonNewGame: TToolButton;
    ToolButtonUndoMove: TToolButton;
    ToolButtonRedoMove: TToolButton;
    ToolButtonDivider1: TToolButton;
    MenuItemAbout: TMenuItem;
    MenuItemSeparator1, MenuItemSeparator2: TMenuItem;
    MenuItemLoadGame: TMenuItem;
    MenuItemSaveGame: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemNewGame: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemGame: TMenuItem;
    MainMenu: TMainMenu;
    PaintBoxBottom, PaintBox: TPaintBox;
    BoardBottom, BoardTop: TPortableNetworkGraphic;
    YMovement: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemBotLevelClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemLoadGameClick(Sender: TObject);
    procedure MenuItemNewCoinPos(Sender: TObject);
    procedure MenuItemNewGameClick(Sender: TObject);
    procedure MenuItemPlayersClick(Sender: TObject);
    procedure MenuItemRedoClick(Sender: TObject);
    procedure MenuItemSaveGameClick(Sender: TObject);
    procedure MenuItemShowCoinPreviewClick(Sender: TObject);
    procedure MenuItemShowStatusBarClick(Sender: TObject);
    procedure MenuItemShowToolBarClick(Sender: TObject);
    procedure MenuItemSkinClick(Sender: TObject);
    procedure MenuItemStart(Sender: TObject);
    procedure MenuItemUndoClick(Sender: TObject);
    procedure PaintBoxBottomPaint(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseLeave(Sender: TObject);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure SmoothXTimer(Sender: TObject);
    procedure YMovementTimer(Sender: TObject);
  private
    Game: TGame;
    DestX: integer;
    LastX: byte;
    LastY: byte;
    DropCoinX: byte;
    DropCoinY: byte;
    CoinSteps: integer;
    CoinFall: boolean;
    procedure NewCoin;
    procedure DelCoin;
    procedure DelAllCoin;
    procedure Display4InARowLine;
    procedure NewLineSegment(NameTag, LineType: string; PartNo, x, y: integer);
    procedure DelAllLineSegment;
    procedure EnableRedo(Enable: boolean);
    procedure EnableUndo(Enable: boolean);
    procedure ShowStatusBar(ShowStatus: boolean);
    procedure ShowToolBar(ShowBar: boolean);
    procedure ResetPlayGround;
    procedure SetPlayGroundSettings;
    procedure ShowPreview;
    procedure SetPreview;
    procedure StartMove;
    procedure FinishMove;
    procedure CoinMove(const Direction: boolean);
    procedure PaintBoxCoinPosition(x: integer); // Calculates position of coin on PaintBox.
    function MouseDownBoundsOk(x, y: integer): boolean;
    procedure ReskinForm;
  end;

var
  PlayGround: TPlayGround;

implementation

{ TPlayGround }
//----------------------------------------------------------------------------//
procedure TPlayGround.FormCreate(Sender: TObject);
begin
  Game:=TGame.Create;
  if Game.Settings.FormLeft=-1000 then PlayGround.Position:=poScreenCenter
  else
    begin
      PlayGround.Left:=Game.Settings.FormLeft;
      PlayGround.Top:=Game.Settings.FormTop;
    end;
  PlayGround.DoubleBuffered:=true; // helps reduce imaging flickering.
  Randomize;
  BoardBottom:=TPortableNetworkGraphic.Create;
  BoardTop:=TPortableNetworkGraphic.Create;
  ToolBarImages:=TCustomImageList.Create(self);
  //ControlStyle := ControlStyle + [csOpaque];
  SetPlayGroundSettings;
  ResetPlayground;
  SetPreview;
  ReskinForm;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.FormDestroy(Sender: TObject);
begin
  BoardBottom.Free;
  BoardTop.Free;
  ToolBarImages.Free;
  Game.Settings.FormLeft:=PlayGround.Left;
  Game.Settings.FormTop:=PlayGround.Top;
  Game.Settings.Write;
  Game.Free;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.MenuItemAboutClick(Sender: TObject);
begin
  ShowMessage(
    'Connect4 v1.4a'                                                +sLineBreak+
    '------------------------------------------------'              +sLineBreak+
    'Author: Nathan Bartram (code and graphics)'                    +sLineBreak+
    'Contact: nathan_bartram@hotmail.co.uk'                         +sLineBreak+
    '------------------------------------------------'              +sLineBreak+
    'Please don''t hesitate to contact me if you have found'        +sLineBreak+
    'a bug or have some good or bad criticisms ;)');
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.MenuItemBotLevelClick(Sender: TObject);
var
  i: byte;
  temp: string;
begin
  for i:=1 to 5 do
    TMenuItem(FindComponent('MenuItemBotLevel'+IntToStr(i))).Checked:=false;
  TMenuItem(Sender).Checked:=true;
  temp:=TMenuItem(Sender).Caption;
  Game.Settings.BotLevel:=StrToInt(temp[Length(temp)]);
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.MenuItemLoadGameClick(Sender: TObject);
var x, y, i, Moves: byte;
begin
  LoadDialog.InitialDir:=GetCurrentDir;
  if LoadDialog.Execute then
    begin
      ResetPlayGround;
      Game.LoadGame(LoadDialog.FileName);
    end
  else Exit;
  Moves:=Game.GetNumberOfMoves;
  Game.ResetNumberOfMoves;
  for i:=1 to Moves-1 do
    begin
      Game.GetMoveFromList(i,x,y);
      NewCoin;
      TImage(FindComponent('Coin'+IntToStr(Game.GetNumberOfMoves))).Left:=PaintBoxSectorsX[x]+PaintBox.Left;
      TImage(FindComponent('Coin'+IntToStr(Game.GetNumberOfMoves))).Top:=PaintBoxSectorsY[y]+PaintBox.Top;
      Game.SwitchPlayer;
    end;
  Game.SwitchPlayer;
  if Game.IsWin then
    begin
      Display4InARowLine;
      PaintBox.Enabled:=false;
      EnableUndo(false);
      EnableRedo(false);
    end
  else if Game.IsGridFull then
    begin
      PaintBox.Enabled:=false;
      EnableUndo(false);
      EnableRedo(false);
    end
  else
    begin
      if Game.GetNumberOfMoves=0 then EnableUndo(false)
      else if (Game.IsCurrentPlayerBot) and (Game.GetNumberOfMoves=1) then EnableUndo(false)
      else if (Game.IsStartPlayerBot) and (Game.GetNumberOfMoves=3) then EnableUndo(false)
      else if ((not Game.IsStartPlayerBot) and (Game.GetNumberOfMoves=2)) and
              ((Game.IsOtherPlayerBot) and (Game.IsCurrentPlayerBot)) then EnableUndo(false)
      else EnableUndo(true);
      Game.SwitchPlayer;
      NewCoin;
      Game.GetMoveFromList(Moves,x,y);
      if (x=0) or (Game.IsCurrentPlayerBot) then EnableRedo(false)
      else EnableRedo(true);
      if Game.IsCurrentPlayerBot then StartMove
      else PaintBox.Enabled:=true;
    end;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.MenuItemNewCoinPos(Sender: TObject);
var
  Temp: string;
  i: byte;
begin
  for i:=0 to 3 do
    TMenuItem(FindComponent('MenuItemNewCoin'+IntToStr(i))).Checked:=false;
  TMenuItem(Sender).Checked:=true;
  Temp:=TMenuItem(Sender).Name;
  Game.Settings.NewCoinPosition:=StrToInt(Temp[Length(Temp)]);
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.MenuItemNewGameClick(Sender: TObject);
begin
  ResetPlayground;
  NewCoin;
  ToolButtonSaveGame.Enabled:=true;
  ToolButtonLoadGame.Enabled:=true;
  EnableUndo(false);
  EnableRedo(false);
  if Game.IsCurrentPlayerBot then StartMove;
  PaintBox.Enabled:=true;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.MenuItemPlayersClick(Sender: TObject);
begin
  with PlayersDialogForm do
    begin
      IsPlayer1Bot:=Game.Settings.IsPlayer1Bot;
      IsPlayer2Bot:=Game.Settings.IsPlayer2Bot;
      Player1LabeledEdit.Text:=Game.Settings.Player1Name;
      Player2LabeledEdit.Text:=Game.Settings.Player2Name;
      Skin:=Game.Settings.Skin;
      UpdateForm;
    end;
  if PlayersDialogForm.ShowModal=mrOk then
    begin
      with Game.Settings do
        begin
          IsPlayer1Bot:=PlayersDialogForm.IsPlayer1Bot;
          IsPlayer2Bot:=PlayersDialogForm.IsPlayer2Bot;
          Player1Name:=PlayersDialogForm.Player1LabeledEdit.Text;
          Player2Name:=PlayersDialogForm.Player2LabeledEdit.Text;
        end;
      if (Game.IsCurrentPlayerBot) and (PaintBox.Enabled) then StartMove;
    end;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.MenuItemRedoClick(Sender: TObject);
var x, y: byte;
begin
  if YMoveMent.Enabled then Exit;
  Game.RedoMove;
  x:=Game.GetLastMoveX;
  y:=Game.GetLastMoveY;
  TImage(FindComponent('Coin'+IntToStr(Game.GetNumberOfMoves))).Left:=PaintBoxSectorsX[x]+PaintBox.Left;
  DropCoinX:=x;
  DropCoinY:=y;
  CoinMove(Down);
  YMovement.Tag:=YMovement.Tag+1;
  Game.GetMoveFromList(Game.GetNumberOfMoves+1,x,y);
  if x=0 then EnableRedo(false);
  EnableUndo(true);
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.MenuItemSaveGameClick(Sender: TObject);
begin
  SaveDialog.InitialDir:=GetCurrentDir;
  if SaveDialog.Execute then Game.SaveGame(SaveDialog.FileName);
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.MenuItemShowCoinPreviewClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked:=not TMenuItem(Sender).Checked;
  Game.Settings.ShowCoinPreview:=TMenuItem(Sender).Checked;
  if TMenuItem(Sender).Checked=true then SetPreview;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.MenuItemShowStatusBarClick(Sender: TObject);
begin
  if TMenuItem(Sender).Checked=true then ShowStatusBar(StrToBool('0'))
  else ShowStatusBar(StrToBool('1'));
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.MenuItemShowToolBarClick(Sender: TObject);
begin
  if TMenuItem(Sender).Checked=true then ShowToolBar(StrToBool('0'))
  else ShowToolBar(StrToBool('1'));
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.MenuItemSkinClick(Sender: TObject);
begin
  MenuItemSkinCrystal.Checked:=false;
  MenuItemSkinCartoon.Checked:=false;
  TMenuItem(Sender).Checked:=true;
  Game.Settings.Skin:=TMenuItem(Sender).Caption;
  ReskinForm;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.MenuItemStart(Sender: TObject);
var
  Temp: string;
  i: byte;
begin
  for i:=0 to 2 do
    TMenuItem(FindComponent('MenuItemStart'+IntTostr(i))).Checked:=false;
  TMenuItem(Sender).Checked:=true;
  Temp:=TMenuItem(Sender).Name;
  Game.Settings.PlayerStart:=StrToInt(Temp[Length(Temp)]);
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.MenuItemUndoClick(Sender: TObject);
begin
  if (Game.GetNumberOfMoves=1) or (YMovement.Enabled) then Exit;
  DelCoin;
  Game.UndoMove;
  SetPreview;
  if not Game.IsCurrentPlayerBot then CoinMove(Up);
  if (Game.GetNumberOfMoves=1) or
     ((Game.IsOtherPlayerBot) and (Game.GetNumberOfMoves=3)) then EnableUndo(false);
  EnableRedo(true);
  CoinPreview.Visible:=false;
  if Game.IsCurrentPlayerBot then MenuItemUndoClick(Sender);
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.PaintBoxBottomPaint(Sender: TObject);
begin
  PaintBoxBottom.Canvas.Draw(0,0,BoardBottom);
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if YMovement.Enabled or SmoothX.Enabled then Exit;
  //SmoothX.Enabled:=false;
  //PaintBox.Enabled:=false; // Stops OnMouseDown and OnMouseMove Events.
  if MouseDownBoundsOk(x,y)=true then
    begin
      DestX:=X;
      PaintBoxCoinPosition(DestX);
      if DropCoinY=0 then
        begin
          PaintBox.Enabled:=true;
          Exit;
        end;
      StartMove;
    end;
  //else PaintBox.Enabled:=true;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.PaintBoxMouseLeave(Sender: TObject);
begin
  CoinPreview.Visible:=false;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if YMovement.Enabled  or (SmoothX.Enabled=true) then
    begin
      if not MouseDownBoundsOk(X,Y) then CoinPreview.Visible:=false;
      DestX:=X;
      Exit;  // Prevents coin x co-orindte chnage while animation in progress
    end;
  if MouseDownBoundsOk(X,Y) then
    begin
      DestX:=X;
      ShowPreview;
      SmoothX.Enabled:=true;
    end
  else
    begin
      CoinPreview.Visible:=false;
      LastX:=0;
      LastY:=0;
    end;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.PaintBoxPaint(Sender: TObject);
begin
  PaintBox.Canvas.Draw(0,0,BoardTop);
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.SmoothXTimer(Sender: TObject);
var
  CurrentX: integer;
  CoinName: string;
begin
  //if YMovement.Enabled=true then Exit;
  if (DestX<34) or (DestX>518) then Exit;
  CoinName:='Coin'+IntToStr(Game.GetNumberOfMoves);
  CurrentX:=TImage(FindComponent(CoinName)).Left;
  CurrentX:=CurrentX-PaintBox.Left;
  case DestX of
    34..66: DestX:=66;
    486..518: DestX:=486;
  end;
  if (CoinSteps=-1) then
  if (CurrentX>DestX+140) or (CurrentX<DestX-140) then
    begin
      CoinSteps:=(CurrentX-(DestX+PaintBox.Left)) div 70;
      if CoinSteps<0 then CoinSteps:=CoinSteps*-1
      else Inc(CoinSteps);
    end
  else
    begin
      TImage(FindComponent(CoinName)).Left:=DestX-32+PaintBox.Left;
      SmoothX.Enabled:=false;
      Exit;
    end;

  Dec(CoinSteps);
  if CurrentX<DestX then
  TImage(FindComponent(CoinName)).Left:=TImage(FindComponent(CoinName)).Left+70
  else
  TImage(FindComponent(CoinName)).Left:=TImage(FindComponent(CoinName)).Left-70;
  if CoinSteps=0 then
  begin
    CoinSteps:=-1;
    TImage(FindComponent(CoinName)).Left:=DestX-32+PaintBox.Left;
    SmoothX.Enabled:=false;
  end;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.YMovementTimer(Sender: TObject);
var
  CoinName: string;
  Speed: byte = 16; //implement into Game.settings
begin
  CoinName:='Coin'+IntToStr(Game.GetNumberOfMoves);
  if CoinSteps=-1 then // Sets number of steps of coin drop.
    begin
      if CoinFall then
        begin
          TImage(FindComponent(CoinName)).Left:=PaintBoxSectorsX[DropCoinX]+PaintBox.Left;
          CoinSteps:=PaintBoxSectorsY[DropCoinY] div Speed;
        end
      else CoinSteps:=PaintBoxSectorsY[Game.GetLastMoveY] div Speed;
    end;
  Dec(CoinSteps);   //number of pixels to drop by
  if CoinFall then TImage(FindComponent(CoinName)).Top:=TImage(FindComponent(CoinName)).Top+Speed
  else TImage(FindComponent(CoinName)).Top:=TImage(FindComponent(CoinName)).Top-Speed;
  if (CoinSteps=0) and CoinFall then
    begin
      CoinSteps:=-1;
      TImage(FindComponent(CoinName)).Top:=PaintBoxSectorsY[DropCoinY]+PaintBox.Top; //implement later
      YMovement.Enabled:=false;
      //Paintbox.Enabled:=false;
      {if not Game.IsCurrentPlayerBot then}
      if YMovement.Tag<>0 then
        begin
          Game.SwitchPlayer;
          NewCoin;
          if (YMovement.Tag=1) and (Game.IsCurrentPlayerBot) then
            begin
              MenuItemRedoClick(Sender);
              YMovement.Tag:=0;
            end
          else YMovement.Tag:=0;
        end
      else FinishMove;
    end
  else if (CoinSteps=0) and not CoinFall then
    begin
      CoinSteps:=-1;
      TImage(FindComponent(CoinName)).Top:=PaintBox.Top-8;
      YMovement.Enabled:=false;
    end;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.NewCoin;
var NewImage: TImage;
begin
  Game.IncNumberOfMoves; // Gives new coin a unique name based on move number.
  NewImage:=TImage.Create(PlayGround);
  with NewImage do
  begin
    Parent:=PlayGround;
    if Game.GetPlayerGo then NewImage.Picture.LoadFromLazarusResource(Game.Settings.Skin+'RedCoin')
    else NewImage.Picture.LoadFromLazarusResource(Game.Settings.Skin+'YellowCoin');
    Width:=64;
    Height:=64;
    case Game.Settings.NewCoinPosition of
      0: NewImage.Left:=PaintBox.Left+PaintBoxSectorsX[((Random(3)+1)*3)-2];
      1: NewImage.Left:=PaintBox.Left+PaintBoxSectorsX[1];
      2: NewImage.Left:=PaintBox.Left+PaintBoxSectorsX[4];
      3: NewImage.Left:=PaintBox.Left+PaintBoxSectorsX[7];
    end;
    Top:=PaintBox.Top-8;
    Transparent:=false;
    Visible:=true;
    Name:='Coin'+IntToStr(Game.GetNumberOfMoves); // Sets unique name.
  end;
  if Game.Settings.ShowCoinPreview then SetPreview;
  PaintBox.BringToFront;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.DelCoin;
begin
  TImage(FindComponent('Coin'+IntToStr(Game.GetNumberOfMoves))).Free;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.DelAllCoin;
var i: byte;
begin
  for i:=1 to Game.GetNumberOfMoves do
  TImage(FindComponent('Coin'+IntToStr(i))).Free;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.Display4InARowLine; // make easy to understand
var
  NewName: byte = 0;
  i, x, y: byte;
begin
  // Paint horizontal if exists
  if Game.WinPosition.IsHorizontalWin then
  begin
    x:=Game.WinPosition.Horizontal.Start.x;
    y:=Game.WinPosition.Horizontal.Start.y;
    Inc(NewName);                                              //paint begining
    NewLineSegment('Line'+IntToStr(NewName),'HorizontalLine',0,
                    PaintBoxSectorsX[x]-13+PaintBox.Left,
                    PaintBoxSectorsY[y]-3+PaintBox.Top);

    for i:=x to Game.WinPosition.Horizontal.Parts-3+x do
    begin
      Inc(NewName);
      Inc(x);
      NewLineSegment('Line'+IntToStr(NewName),'HorizontalLine',1,
                      PaintBoxSectorsX[x]-2+PaintBox.Left,
                      PaintBoxSectorsY[y]-3+PaintBox.Top);
    end;
    Inc(NewName);

    Inc(x);
    NewLineSegment('Line'+IntToStr(NewName),'HorizontalLine',2,
                    PaintBoxSectorsX[x]-2+PaintBox.Left,
                    PaintBoxSectorsY[y]-3+PaintBox.Top);
  end;
 //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Paint vertical line
  if Game.WinPosition.IsVerticalWin then
  begin
    x:=Game.WinPosition.Vertical.Start.x;
    y:=Game.WinPosition.Vertical.Start.y;
    Inc(NewName);
    NewLineSegment('Line'+IntToStr(NewName),'VerticalLine',0,
                      PaintBoxSectorsX[x]-3+PaintBox.Left,
                      PaintBoxSectorsY[y]-9+PaintBox.Top);

    for i:=y to Game.WinPosition.Vertical.Parts+y-3 do
    begin
      Inc(NewName);
      Inc(y);
      NewLineSegment('Line'+IntToStr(NewName),'VerticalLine',1,
                      PaintBoxSectorsX[x]-3+PaintBox.Left,
                      PaintBoxSectorsY[y]-3+PaintBox.Top);
    end;
    Inc(NewName);
    Inc(y);
    NewLineSegment('Line'+IntToStr(NewName),'VerticalLine',2,
                      PaintBoxSectorsX[x]-3+PaintBox.Left,
                      PaintBoxSectorsY[y]-3+PaintBox.Top);

  end;
 //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 // Paint Left Diagonal
  if Game.WinPosition.IsLeftDiagonalWin then
  begin
    x:=Game.WinPosition.LeftDiagonal.Start.x;
    y:=Game.WinPosition.LeftDiagonal.Start.y;
    Inc(NewName);
    NewLineSegment('Line'+IntToStr(NewName),'LeftDiagonalLine',0,
                      PaintBoxSectorsX[x]-14+PaintBox.Left,
                      PaintBoxSectorsY[y]-9+PaintBox.Top);
    for i:=y to Game.WinPosition.LeftDiagonal.Parts+y-3 do
    begin
      Inc(NewName);
      Inc(y);
      Inc(x);
      NewLineSegment('Line'+IntToStr(NewName),'LeftDiagonalLine',1,
                      PaintBoxSectorsX[x]-3+PaintBox.Left,
                      PaintBoxSectorsY[y]-3+PaintBox.Top);
    end;
      Inc(NewName);
      Inc(y);
      Inc(x);
      NewLineSegment('Line'+IntToStr(NewName),'LeftDiagonalLine',2,
                      PaintBoxSectorsX[x]-3+PaintBox.Left,
                      PaintBoxSectorsY[y]-3+PaintBox.Top);

  end;

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 // Paint Right Diagonal
  if Game.WinPosition.IsRightDiagonalWin then
  begin
    x:=Game.WinPosition.RightDiagonal.Start.x;
    y:=Game.WinPosition.RightDiagonal.Start.y;
    Inc(NewName);
    NewLineSegment('Line'+IntToStr(NewName),'RightDiagonalLine',0,
                      PaintBoxSectorsX[x]-3+PaintBox.Left,
                      PaintBoxSectorsY[y]-38+PaintBox.Top);
    for i:=x downto x-(Game.WinPosition.RightDiagonal.Parts-3) do
    begin
      Inc(NewName);
      Inc(y);
      Dec(x);
      NewLineSegment('Line'+IntToStr(NewName),'RightDiagonalLine',1,
                      PaintBoxSectorsX[x]-3+PaintBox.Left,
                      PaintBoxSectorsY[y]-38+PaintBox.Top);
    end;
      Inc(NewName);
      Inc(y);
      Dec(x);
      NewLineSegment('Line'+IntToStr(NewName),'RightDiagonalLine',2,
                      PaintBoxSectorsX[x]-14+PaintBox.Left,
                      PaintBoxSectorsY[y]-32+PaintBox.Top);
  end;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.NewLineSegment(NameTag, LineType: string; PartNo, x, y: integer); // make easy to understand
var
  NewImage: TImage;
  temp: string;
  w, h: integer;
begin
  case LineType[1] of
    'H': begin
           w:=81;
           h:=70;
         end;
    'V': begin
           w:=70;
           h:=76;
         end;
    'L','R': begin
               w:=105;
               h:=w;
             end;
  end;

  NewImage:=TImage.Create(PlayGround);
  with NewImage do
    begin
      Parent:=PlayGround;
      Left:=x;
      Top:=y;
      Width:=w;
      Height:=h;
      Visible:=true;
      Transparent:=false;
      Name:=NameTag;
    end;
  case PartNo of
    0: temp:='Top';
    1: temp:='Body';
    2: temp:='Bottom';
  end;
  temp:=LineType+temp;
  TImage(FindComponent(NameTag)).Picture.LoadFromLazarusResource(Game.Settings.Skin+temp);
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.DelAllLineSegment;
var i, max: byte;
begin
  max:=Game.WinPosition.Horizontal.Parts+Game.WinPosition.Vertical.Parts+
       Game.WinPosition.LeftDiagonal.Parts+Game.WinPosition.RightDiagonal.Parts;
  try
    for i:=1 to max do TImage(FindComponent('Line'+IntToStr(i))).Free;
  except
    // Do nothing.
  end;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.EnableRedo(Enable: boolean);
begin
  MenuItemRedo.Enabled:=Enable;
  ToolButtonRedoMove.Enabled:=Enable;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.EnableUndo(Enable: boolean);
begin
  MenuItemUndo.Enabled:=Enable;
  ToolButtonUndoMove.Enabled:=Enable;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.ShowStatusBar(ShowStatus: boolean);
begin
  if ShowStatus then Playground.Height:=Playground.Height+StatusBar.Height
  else Playground.Height:=Playground.Height-StatusBar.Height;
  StatusBar.Visible:=ShowStatus;
  MenuItemShowStatusBar.Checked:=ShowStatus;
  Game.Settings.ShowStatusBar:=ShowStatus;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.ShowToolBar(ShowBar: boolean);
var i: byte;
begin
  if ToolBar.Visible=ShowBar then Exit;
  if ShowBar then
    begin
      PaintBox.Top:=PaintBox.Top+ToolBar.Height;
      for i:=1 to Game.GetNumberOfMoves do
      TImage(FindComponent('Coin'+IntTostr(i))).Top:=
      TImage(FindComponent('Coin'+IntTostr(i))).Top+ToolBar.Height;
      Playground.Height:=Playground.Height+ToolBar.Height;
    end
  else
    begin
      PaintBox.Top:=PaintBox.Top-ToolBar.Height;
      for i:=1 to Game.GetNumberOfMoves do
      TImage(FindComponent('Coin'+IntTostr(i))).Top:=
      TImage(FindComponent('Coin'+IntTostr(i))).Top-ToolBar.Height;
      Playground.Height:=Playground.Height-ToolBar.Height;
    end;
  MenuItemShowToolBar.Checked:=ShowBar;
  ToolBar.Visible:=ShowBar;
  PaintBoxBottom.Top:=PaintBox.Top;
  Game.Settings.ShowToolBar:=ShowBar;
  if Game.WinPosition.IsHorizontalWin or Game.WinPosition.IsVerticalWin or
     Game.WinPosition.IsLeftDiagonalWin or Game.WinPosition.IsRightDiagonalWin then
    begin
      DelAllLineSegment;
      Display4InARowLine;
    end;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.ResetPlayGround;
begin
  DestX:=-1;
  CoinSteps:=-1;
  LastX:=0;
  LastY:=0;
  DelAllCoin;
  DelAllLineSegment;
  Game.NewGame;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.SetPlayGroundSettings;
begin
  TMenuItem(FindComponent('MenuItemStart'+IntToStr(Game.Settings.PlayerStart))).Checked:=true;
  TMenuItem(FindComponent('MenuItemNewCoin'+IntToStr(Game.Settings.NewCoinPosition))).Checked:=true;
  ShowToolBar(Game.Settings.ShowToolBar);
  if not Game.Settings.ShowStatusBar then ShowStatusBar(Game.Settings.ShowStatusBar)
  else MenuItemShowStatusBar.Checked:=true;
  MenuItemShowCoinPreview.Checked:=Game.Settings.ShowCoinPreview;
  TMenuItem(FindComponent('MenuItemBotLevel'+IntToStr(Game.Settings.BotLevel))).Checked:=true;
  TMenuItem(FindComponent('MenuItemSkin'+Game.Settings.Skin)).Checked:=true;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.ShowPreview;
begin
  if not Game.Settings.ShowCoinPreview then Exit;
  PaintBoxCoinPosition(DestX);
  if ((LastX=DropCoinX) and (LastY=DropCoinY)) then
    if DropCoinY=0 then
      begin
        CoinPreview.Visible:=false;
        Exit;
      end;
  CoinPreview.Left:=PaintBox.Left+PaintBoxSectorsX[DropCoinX];
  CoinPreview.Top:=PaintBox.Top+PaintBoxSectorsY[DropCoinY];
  CoinPreview.Visible:=true;
  LastX:=DropCoinX;
  LastY:=DropCoinY;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.SetPreview;
begin
  if Game.GetPlayerGo then CoinPreview.Picture.LoadFromLazarusResource(Game.Settings.Skin+'RedOverlay')
  else CoinPreview.Picture.LoadFromLazarusResource(Game.Settings.Skin+'YellowOverlay');
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.StartMove;
var XMove: byte;
begin
  //Game.GetMoveFromList(Game.GetNumberOfMoves+1,XMove,y);
  if Game.IsCurrentPlayerBot then
    begin
      XMove:=Game.GetBotMove;
      PaintBoxCoinPosition(PaintBoxSectorsX[XMove]);
      Game.SetMove(XMove,DropCoinY);
      CoinMove(Down);
    end
  else
    begin
      Game.SetMove(DropCoinX,DropCoinY);
     // if Game.GetPlayerNumber(Game.GetPlayerGo) = 1 then game.GetBotMove;///
      EnableRedo(false);
      CoinMove(Down);
    end;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.FinishMove;
begin
  if Game.IsWin then
    begin
      Display4InARowLine;
      if Game.Settings.ShowCoinPreview then CoinPreview.Visible:=false;
      PaintBox.Enabled:=false;
      EnableUndo(false);
      if Game.GetPlayerGo then
      StatusBar.Panels[0].Text:='Player 1 ('+Game.Settings.Player1Name+') wins!'
      else
      StatusBar.Panels[0].Text:='Player 2 ('+Game.Settings.Player2Name+') wins!'
    end
  else if Game.IsGridFull then
    begin
      CoinPreview.Visible:=false;
      PaintBox.Enabled:=false;
      EnableUndo(false);
      StatusBar.Panels[0].Text:='Draw!';
    end
  else
    begin
      if (Game.IsCurrentPlayerBot) and (Game.GetNumberOfMoves=1) then EnableUndo(false)
      else if (Game.IsStartPlayerBot) and (Game.GetNumberOfMoves=3) then EnableUndo(false)
      else if ((not Game.IsStartPlayerBot) and (Game.GetNumberOfMoves=2)) and
              ((Game.IsOtherPlayerBot) and (Game.IsCurrentPlayerBot)) then EnableUndo(false)
      else EnableUndo(true);
      Game.SwitchPlayer;
      NewCoin;
      if Game.IsCurrentPlayerBot then StartMove
      else
        begin
          PaintBox.Enabled:=true;
          SmoothX.Enabled:=true;
        end;
    end;
end;
//----------------------------------------------------------------------------//
procedure TPlayGround.CoinMove(const Direction: boolean);
begin
  CoinFall:=Direction;
  YMovement.Enabled:=true;
end;
//----------------------------------------------------------------------------/
procedure TPlayGround.PaintBoxCoinPosition(x: integer);
var i: byte;
begin
  if (DestX<34) or (DestX>518) then
    DestX:=TImage(FindComponent('Coin'+IntToStr(Game.GetNumberOfMoves))).Left-PaintBox.Left;
  for i:=1 to 8 do
    if x<PaintBoxSectorsX[i]-3 then Break;   // Finds which sector x is in.
  DropCoinX:=i-1;                          // Stores x sector.
  DropCoinY:=Game.GetDropLocation(DropCoinX);  // Finds y destination of falling coin from MovesGrid.
end;
//----------------------------------------------------------------------------//
function TPlayGround.MouseDownBoundsOk(x, y: integer): boolean;
begin
  if (x<PaintBoxSectorsX[1]) or (x>PaintBoxSectorsX[8]) or
     (y<PaintBoxSectorsX[1]) or (y>PaintBoxSectorsY[7]) then Result:=false
  else Result:=true;
end;                
//----------------------------------------------------------------------------//
procedure TPlayGround.ReskinForm;
var
  i: byte;
  Player: boolean;
begin
  BoardTop.LoadFromLazarusResource(Game.Settings.Skin+'BoardTop');
  BoardBottom.LoadFromLazarusResource(Game.Settings.Skin+'BoardBottom');
  Player:=Game.GetFirstPlayer;
  for i:=1 to Game.GetNumberOfMoves do
    begin
      if Player then
        TImage(FindComponent('Coin'+IntToStr(i))).Picture.LoadFromLazarusResource(Game.Settings.Skin+'RedCoin')
      else
        TImage(FindComponent('Coin'+IntToStr(i))).Picture.LoadFromLazarusResource(Game.Settings.Skin+'YellowCoin');
      Player:=not Player;
    end;
  PaintBoxBottom.Repaint;
  if Game.WinPosition.IsHorizontalWin or Game.WinPosition.IsLeftDiagonalWin or
     Game.WinPosition.IsRightDiagonalWin or Game.WinPosition.IsVerticalWin then
    begin
      DelAllLineSegment;
      Display4InARowLine;
    end;
  SetPreview;
  ToolBarImages.Clear;
  ToolBarImages.Width:=32;
  ToolBarImages.Height:=32;
  ToolBarImages.AddLazarusResource(Game.Settings.Skin+'NewGameIcon');
  ToolBarImages.AddLazarusResource(Game.Settings.Skin+'SaveIcon');
  ToolBarImages.AddLazarusResource(Game.Settings.Skin+'OpenIcon');
  ToolBarImages.AddLazarusResource(Game.Settings.Skin+'BackIcon');
  ToolBarImages.AddLazarusResource(Game.Settings.Skin+'ForwardIcon');
  ToolBar.Images:=ToolBarImages;
end;
//----------------------------------------------------------------------------//

initialization
  {$I main.lrs}

end.
