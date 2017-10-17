program ScreenCropper;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainFormUnit, jwawinbase;

{$R *.res}

var
  mutexHandle: QWORD;

begin
  mutexHandle := CreateMutex(nil, true, 'ScreenCropperMutex');

  if (GetLastError() <> 183) then
  begin
    Application.Title:='Screen Cropper';
    RequireDerivedFormResource:=True;

    Application.Initialize;
    Application.ShowMainForm:=false;
    Application.CreateForm(TSelectionForm, SelectionForm);
    Application.Run;
  end;

  ReleaseMutex(mutexHandle);
  CloseHandle(mutexHandle);
end.

