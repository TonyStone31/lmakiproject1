unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Graphics,
  LCLType,
  MouseAndKeyInput, StdCtrls,
  SysUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnAltTab: TButton;
    btnStartTest: TButton;
    btnReType: TButton;
    btnMoveMouseClick: TButton;
    Button1: TButton;
    lblKeyCode: TLabel;
    memTestSource: TMemo;
    memTestDestination: TMemo;
    memTextToSim: TMemo;
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    procedure btnAltTabClick(Sender: TObject);
    procedure btnStartTestClick(Sender: TObject);
    procedure btnReTypeClick(Sender: TObject);
    procedure btnMoveMouseClickClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure memTextToSimKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    DrawingActive: boolean;
    LastDrawingPoint: TPoint;
    BitmapCanvasDemo: TBitmap;

    procedure RunMouseDemo(Data: PtrInt);
    procedure RunStartTest(Data: PtrInt);
    procedure RunReType(Data: PtrInt);
    procedure TypeUnicodeCharacters(Data: PtrInt);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnStartTestClick(Sender: TObject);
begin
  Application.QueueAsyncCall(@RunStartTest, 0);
end;

procedure TForm1.RunStartTest(Data: PtrInt);
begin
  memTestDestination.Clear;
  memTestDestination.SetFocus;
  KeyInput.PressString(memTestSource.Lines.Text);
  Sleep(1000); // Simulates the test delay

  if memTestDestination.Lines.Text = memTestSource.Lines.Text then
    memTestDestination.Lines.Add('Test Passed')
  else
    memTestDestination.Lines.Add('Test Failed!');
end;

procedure TForm1.btnAltTabClick(Sender: TObject);
var
  i: integer;
begin
  KeyInput.Down(VK_MENU);

  for i := 0 to 5 do
  begin
    KeyInput.Press(VK_TAB);
    Sleep(400);
  end;

  KeyInput.Up(VK_MENU);
end;

procedure TForm1.btnMoveMouseClickClick(Sender: TObject);
begin
  Application.QueueAsyncCall(@RunMouseDemo, 0);
end;

procedure TForm1.RunMouseDemo(Data: PtrInt);
var
  cornerPoints: array[0..4] of TPoint;
  adjustedPoints: array[0..4] of TPoint;
  paintBoxScreenPos: TPoint;
  paintBoxWidth, paintBoxHeight: integer;
  i, n: integer;
  angle: double;
  offsets: integer = 5;
  radius: integer = 50; // Radius of the circle
  offset: integer = 75; // Offset from the corners inward
  steps: integer = 4;   // Number of steps for the circle
begin
  // Get the PaintBox1's desktop coordinates
  paintBoxScreenPos := PaintBox1.ClientToScreen(Point(0, 0));

  paintBoxWidth := PaintBox1.Width;
  paintBoxHeight := PaintBox1.Height;

  // Borders
  for i := 0 to 1 do
  begin
    MouseInput.Move([], paintBoxScreenPos.X + offsets, paintBoxScreenPos.Y + offsets, 200); // Start at top-left
    MouseInput.Down(mbLeft, []);
    MouseInput.Move([], paintBoxScreenPos.X + PaintBox1.Width - offsets, paintBoxScreenPos.Y + offsets, 200); // Top edge
    MouseInput.Move([], paintBoxScreenPos.X + PaintBox1.Width - offsets, paintBoxScreenPos.Y + PaintBox1.Height - offsets, 200); // Right edge
    MouseInput.Move([], paintBoxScreenPos.X + offsets, paintBoxScreenPos.Y + PaintBox1.Height - offsets, 200); // Bottom edge
    MouseInput.Move([], paintBoxScreenPos.X + offsets, paintBoxScreenPos.Y + offsets, 200); // Left edge
    MouseInput.Up(mbLeft, []);
    Inc(offsets, 15);
  end;

  adjustedPoints[0] := Point(paintBoxScreenPos.X + offset, paintBoxScreenPos.Y + offset); // Top-left inward
  adjustedPoints[1] := Point(paintBoxScreenPos.X + paintBoxWidth - offset, paintBoxScreenPos.Y + offset); // Top-right inward
  adjustedPoints[2] := Point(paintBoxScreenPos.X + offset, paintBoxScreenPos.Y + paintBoxHeight - offset); // Bottom-left inward
  adjustedPoints[3] := Point(paintBoxScreenPos.X + paintBoxWidth - offset, paintBoxScreenPos.Y + paintBoxHeight - offset); // Bottom-right inward
  adjustedPoints[4] := Point(paintBoxScreenPos.X + (paintBoxWidth div 2), paintBoxScreenPos.Y + (paintBoxHeight div 2)); // Center


  for i := 0 to High(adjustedPoints) do
  begin
    // Start position of the circle
    MouseInput.Move([], adjustedPoints[i].X + radius, adjustedPoints[i].Y, 1000);
    MouseInput.Down(mbLeft, []);

    // circular motion
    for n := 0 to steps do
    begin
      angle := n * (2 * Pi / steps);
      MouseInput.Move([],
        adjustedPoints[i].X + Round(radius * Cos(angle)),
        adjustedPoints[i].Y + Round(radius * Sin(angle)), (200 div steps));
    end;
    steps := steps * 2;
    MouseInput.Up(mbLeft, []);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Application.QueueAsyncCall(@TypeUnicodeCharacters, 0);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BitmapCanvasDemo := TBitmap.Create;
  BitmapCanvasDemo.SetSize(PaintBox1.Width, PaintBox1.Height);
  BitmapCanvasDemo.Canvas.Brush.Color := clWhite;
  BitmapCanvasDemo.Canvas.FillRect(BitmapCanvasDemo.Canvas.ClipRect);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  BitmapCanvasDemo := TBitmap.Create;
  BitmapCanvasDemo.SetSize(PaintBox1.Width, PaintBox1.Height);
  BitmapCanvasDemo.Canvas.Brush.Color := clWhite;
  BitmapCanvasDemo.Canvas.FillRect(BitmapCanvasDemo.Canvas.ClipRect);
end;

procedure TForm1.TypeUnicodeCharacters(Data: PtrInt);
var
  i, counter: integer;
begin
  memTestDestination.Clear;
  memTestDestination.SetFocus;

  counter := 0;
  for i := 14000 to 14020 do
  begin
    KeyInput.PressUnicodeChar(i);
    Inc(counter);

    if counter = 30 then
    begin
      KeyInput.Press(VK_RETURN);
      counter := 0;
    end;

    Sleep(50); // Optional delay for pacing
  end;
end;

procedure TForm1.memTextToSimKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  lblKeyCode.Caption := 'Key Code: ' + IntToStr(Key) + ' (Hex: $' + IntToHex(Key, 4) + ')';
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    DrawingActive := True;
    LastDrawingPoint := Point(X, Y);
  end;
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if DrawingActive then
  begin
    BitmapCanvasDemo.Canvas.Pen.Color := clRed;
    BitmapCanvasDemo.Canvas.Pen.Width := 2;
    BitmapCanvasDemo.Canvas.Line(LastDrawingPoint.X, LastDrawingPoint.Y, X, Y);

    PaintBox1.Invalidate;
    LastDrawingPoint := Point(X, Y);
  end;
end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
    DrawingActive := False;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0, 0, BitmapCanvasDemo);
end;

procedure TForm1.btnReTypeClick(Sender: TObject);
var
  i: integer;
begin
  for i := 5 downto 1 do
  begin
    btnReType.Caption := Format('Retype in %d Seconds', [i]);
    Application.ProcessMessages;
    Sleep(1000);
  end;
  Application.QueueAsyncCall(@RunReType, 0);
  btnReType.Caption := 'Retype in 5 Seconds';
end;

procedure TForm1.RunReType(Data: PtrInt);
begin
  KeyInput.PressString(memTextToSim.Lines.Text);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  mouseX, mouseY: integer;
begin
  mouseX := Mouse.CursorPos.X;
  mouseY := Mouse.CursorPos.Y;
  Form1.Caption := 'Mouse position: (' + IntToStr(mouseX) + ', ' + IntToStr(mouseY) + ')';
end;

end.
