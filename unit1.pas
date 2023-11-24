unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MouseAndKeyInput, LCLType;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnStartTest: TButton;
    btnReType: TButton;
    btnMoveMouseClick: TButton;
    memTestSource: TMemo;
    memTestDestination: TMemo;
    memTextToSim: TMemo;
    Timer1: TTimer;
    procedure btnStartTestClick(Sender: TObject);
    procedure btnReTypeClick(Sender: TObject);
    procedure btnMoveMouseClickClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnStartTestClick(Sender: TObject);
begin
  memTestDestination.Clear;
  memTestDestination.SetFocus;
  KeyInput.PressString(memTestSource.Lines.Text);
  if memTestDestination.Lines.Text = memTestSource.Lines.Text then memTestDestination.Lines.Add('Test Passed')
  else
    memTestDestination.Lines.Add('Test Failed!');

end;

procedure TForm1.btnMoveMouseClickClick(Sender: TObject);
const
  LoopCount = 4;
var
  i: integer;
begin
  for i := 1 to LoopCount do
  begin
    MouseInput.Move([], 3792, 4070, 275);
    MouseInput.Move([], 3800, 4030, 150);
    Sleep(700);
    MouseInput.Click(mbLeft, [], 3792, 4030);
    Sleep(700);

    MouseInput.Move([], 3542, 3973, 150);
    Sleep(700);
    MouseInput.Click(mbLeft, [], 3542, 3973);
    Sleep(700);

    MouseInput.Move([], 2306, 2565, 150);
    Sleep(700);
    MouseInput.Click(mbLeft, [], 2306, 2565);
    Sleep(700);
  end;

end;

procedure TForm1.btnReTypeClick(Sender: TObject);
begin
  btnReType.Caption := 'Typing in 3 Seconds.....';
  Application.ProcessMessages;
  sleep(3000);
  KeyInput.PressString(memTextToSim.Lines.Text);
  btnReType.Caption := 'Retype in 3 Seconds';

end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  mouseX, mouseY: integer;
begin
  mouseX := Mouse.CursorPos.X;
  mouseY := Mouse.CursorPos.Y;
  //display the result
  form1.Caption := 'Mouse position: (' + IntToStr(mouseX) + ', ' +
    IntToStr(mouseY) + ')';

end;

end.
