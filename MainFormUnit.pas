unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, jwawinuser, windows;

type

  { TSelectionForm }

  TSelectionForm = class(TForm)
    ChangeItem: TMenuItem;
    ChangeMonitorItem: TMenuItem;
    QuitItem: TMenuItem;
    SelectionShape: TShape;
    TrayPopup: TPopupMenu;
    TrayIcon: TTrayIcon;

    procedure ChangeItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure QuitItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SelectionForm: TSelectionForm;

  keyNeeded: DWORD;

  kbdHook: HHOOK;
  mouseHook: HHOOK;

  takingScreenShot: boolean;
  selectingScreenShotArea: boolean;
  changingShortcutCombination: boolean;

  currTopLeftX, currTopLeftY: longInt;

  currMonitor: ^TMonitor;

implementation

{$R *.lfm}

{ TSelectionForm }

function isBitSet(const AValueToCheck, ABitIndex: LongInt): Boolean;
begin
  Result := AValueToCheck and (1 shl ABitIndex) <> 0;
end;

function kbdHookProc(nCode: longInt; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  KBInfoStruct: ^KBDLLHOOKSTRUCT;
  vKeyCode: LONG;

  isCtrlDown: boolean;
  isAltDown: boolean;

  drawReg: HRGN;
begin
  if (nCode < 0) or ((wParam <> WM_KEYDOWN) and (wParam <> WM_SYSKEYDOWN)) then
  begin
    result := CallNextHookEx(0, nCode, wParam, lParam);
    exit;
  end;

  KBInfoStruct := PKBDLLHOOKSTRUCT(lParam);

  vKeyCode := KBInfoStruct^.vkCode;

  if (vKeyCode = VK_F1) then
  begin
    Application.Terminate;
  end;

  if ((changingShortcutCombination) and (vKeyCode = VK_RETURN)) then
  begin
    changingShortcutCombination := false;
    SelectionForm.TrayIcon.BalloonTitle:='Changing shortcut combination';
    SelectionForm.TrayIcon.BalloonHint:='Shortcut was succesfuly changed';
    SelectionForm.TrayIcon.ShowBalloonHint;
    result := CallNextHookEx(0, nCode, wParam, lParam);
    exit;
  end;

  if (changingShortcutCombination) then
  begin
    keyNeeded := vKeyCode;
    result := CallNextHookEx(0, nCode, wParam, lParam);
    exit;
  end;

  isCtrlDown := (isBitSet(GetKeyState(VK_CONTROL), 15));
  isAltDown := (isBitSet(GetKeyState(VK_MENU), 15));

  if ((isCtrlDown) and (isAltDown) and (vKeyCode = keyNeeded)) then
  begin
    if ((SelectionForm.Left <> Screen.DesktopLeft) or (SelectionForm.Top <> Screen.DesktopTop)) then
    begin
      SelectionForm.Left := Screen.DesktopLeft;
      SelectionForm.Top := Screen.DesktopTop;
    end;

    SelectionForm.Width:=Screen.DesktopWidth;
    SelectionForm.Height:=Screen.DesktopHeight;

    drawReg := CreateRectRgn(0,0,SelectionForm.Width,SelectionForm.Height);
    SetWindowRgn(SelectionForm.Handle, drawReg, true);

    SelectionForm.Visible:=true;
    // this second check is weird, i know. but i have no idea to why after setting visible to false, my Left field is being cut in half..
    if ((SelectionForm.Left <> Screen.DesktopLeft) or (SelectionForm.Top <> Screen.DesktopTop)) then
    begin
      SelectionForm.Left := Screen.DesktopLeft;
      SelectionForm.Top := Screen.DesktopTop;
    end;

    takingScreenShot := true;
  end;

  result := CallNextHookEx(0, nCode, wParam, lParam);
end;

function mouseHookProc(nCode: longInt; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  screenContext: HDC;
  compatibleContext: HDC;

  screenWidth: longInt;
  screenHeight: longInt;

  screenBitmap: HBITMAP;
  tempScreenBitmap: HBITMAP;

  drawReg: HRGN;

  mouseScreen, mouseClient: TPoint;

  copyBitMapPointTopLeft, tempShapePoint: TPoint;
begin
  if ((nCode < 0) or ((wParam <> WM_LBUTTONDOWN) and (wParam <> WM_MOUSEMOVE) and (wParam <> WM_LBUTTONUP)) or not(takingScreenShot)) then
  begin
    result := CallNextHookEx(0, nCode, wParam, lParam);
    exit;
  end;

  mouseScreen.x := Mouse.CursorPos.x;
  mouseScreen.y := Mouse.CursorPos.y;

  mouseClient := SelectionForm.ScreenToClient(mouseScreen);




  if (wParam = WM_LBUTTONDOWN) then
  begin
    SelectionForm.SelectionShape.Visible:=true;

    currTopLeftX := mouseClient.x;
    currTopLeftY := mouseClient.y;

    SelectionForm.SelectionShape.Left:=mouseClient.x;
    SelectionForm.SelectionShape.Top:=mouseClient.y;

    selectingScreenShotArea := true;
  end
  else if ((wParam = WM_LBUTTONUP) and (selectingScreenShotArea)) then
  begin
    SelectionForm.SelectionShape.Visible:=false;
    SelectionForm.Refresh;
    SelectionForm.Repaint;
    SelectionForm.Visible:=false;

    screenContext := GetDC(0);
    compatibleContext := CreateCompatibleDC(screenContext);

    screenWidth := SelectionForm.SelectionShape.Width;
    screenHeight := SelectionForm.SelectionShape.Height;

    screenBitmap := CreateCompatibleBitmap(screenContext, screenWidth, screenHeight);
    tempScreenBitmap := SelectObject(compatibleContext, screenBitmap);

    tempShapePoint.x:=SelectionForm.SelectionShape.Left;
    tempShapePoint.y:=SelectionForm.SelectionShape.Top;

    copyBitMapPointTopLeft := SelectionForm.ClientToScreen(tempShapePoint);
    BitBlt(compatibleContext, 0, 0, screenWidth, screenHeight, screenContext, copyBitMapPointTopLeft.x, copyBitMapPointTopLeft.y, SRCCOPY);

    screenBitmap := SelectObject(compatibleContext, tempScreenBitmap);

    OpenClipboard(0);
    EmptyClipboard();
    SetClipboardData(CF_BITMAP, screenBitmap);
    CloseClipboard();

    DeleteDC(compatibleContext);
    ReleaseDC(0, screenContext);
    DeleteObject(screenBitmap);

    selectingScreenShotArea := false;
    takingScreenShot := false;

    SelectionForm.SelectionShape.Width:=0;
    SelectionForm.SelectionShape.Height:=0;

    drawReg := CreateRectRgn(0,0,0,0);

    SetWindowRgn(SelectionForm.Handle, drawReg, true);
  end
  else if ((wParam = WM_MOUSEMOVE) and (selectingScreenShotArea)) then
  begin
    if (currTopLeftX < mouseClient.x) then
    begin
      SelectionForm.SelectionShape.Left:=currTopLeftX;
    end
    else
    begin
      SelectionForm.SelectionShape.Left:=mouseClient.x;
    end;

    if (currTopLeftY < mouseClient.y) then
    begin
      SelectionForm.SelectionShape.Top:=currTopLeftY;
    end
    else
    begin
      SelectionForm.SelectionShape.Top:=mouseClient.y;
    end;

    SelectionForm.SelectionShape.Width:=Abs(currTopLeftX - mouseClient.x);
    SelectionForm.SelectionShape.Height:=Abs(currTopLeftY - mouseClient.y);

    drawReg := CreateRectRgn(SelectionForm.SelectionShape.Left,
                             SelectionForm.SelectionShape.Top,
                             SelectionForm.SelectionShape.Left + SelectionForm.SelectionShape.Width,
                             SelectionForm.SelectionShape.Top + SelectionForm.SelectionShape.Height
                             );

    SetWindowRgn(SelectionForm.Handle, drawReg, true);
  end;

  result := CallNextHookEx(0, nCode, wParam, lParam);
end;

procedure TSelectionForm.FormCreate(Sender: TObject);
var
  drawReg: HRGN;
begin
  drawReg := CreateRectRgn(0,0,0,0);

  SetWindowRgn(Handle, drawReg, true);

  Canvas.Brush.Color:=clWhite;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Pen.Style:=psClear;

  keyNeeded := VK_C;

  kbdHook := SetWindowsHookEx(WH_KEYBOARD_LL, @kbdHookProc, HInstance, 0);
  mouseHook := SetWindowsHookEx(WH_MOUSE_LL, @mouseHookProc, HInstance, 0);

  takingScreenShot := false;
  selectingScreenShotArea := false;
  changingShortcutCombination := false;
end;

procedure TSelectionForm.ChangeItemClick(Sender: TObject);
begin
  changingShortcutCombination := true;

  TrayIcon.BalloonTitle:='Changing shortcut combination';
  TrayIcon.BalloonHint:='Perform any combination of keys and press Enter';
  TrayIcon.ShowBalloonHint;
end;

procedure TSelectionForm.FormDestroy(Sender: TObject);
begin
  UnhookWindowsHookEx(kbdHook);
  UnhookWindowsHookEx(mouseHook);
end;

procedure TSelectionForm.QuitItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.

