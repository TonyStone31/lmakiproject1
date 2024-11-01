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
    Button1: TButton;
    lblKeyCode: TLabel;
    memTestSource: TMemo;
    memTestDestination: TMemo;
    memTextToSim: TMemo;
    Timer1: TTimer;
    procedure btnStartTestClick(Sender: TObject);
    procedure btnReTypeClick(Sender: TObject);
    procedure btnMoveMouseClickClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure memTestSourceChange(Sender: TObject);
    procedure memTextToSimKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
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
  Application.ProcessMessages;
  Sleep(1000);

  if memTestDestination.Lines.Text = memTestSource.Lines.Text then memTestDestination.Lines.Add('Test Passed')
  else
    memTestDestination.Lines.Add('Test Failed!');

end;

procedure TForm1.btnMoveMouseClickClick(Sender: TObject);
const
  LoopCount = 1;
var
  i: integer;
begin
  for i := 1 to LoopCount do
  begin
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

procedure TForm1.Button1Click(Sender: TObject);
var
  i, counter: Integer;
begin
  memTestDestination.Clear;
  memTestDestination.SetFocus;

  counter := 0;
  // Loop through a range of Unicode characters
  for i := 140000 to 142000 do
  begin
    KeyInput.PressUnicodeChar(i);
    Inc(counter);

    // Every 30 characters, press the return key
    if counter = 30 then
    begin
      KeyInput.Press(VK_RETURN);
      counter := 0; // Reset the counter
    end;

    Application.ProcessMessages; // Keep the UI responsive
  end;
end;

procedure TForm1.memTestSourceChange(Sender: TObject);
begin

end;

procedure TForm1.memTextToSimKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  lblKeyCode.Caption := 'Key Code: ' + IntToStr(Key) +
                        ' (Hex: $' + IntToHex(Key, 4) + ')';
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
