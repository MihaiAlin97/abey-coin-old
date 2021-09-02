unit frmStatistics;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  TAGraph, TASources, TASeries;

type

  { TStatisticsForm }

  TStatisticsForm = class(TForm)
    BlockTimeChart: TChart;
    BlockLineSeries: TLineSeries;
    Label10: TLabel;
    Label11: TLabel;
    lblBlockTime: TLabel;
    lblStatus: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    lblAccounts: TLabel;
    lblBlocksFound: TLabel;
    lblPendingOperations: TLabel;
    lblBlocks: TLabel;
    lblMiningTarget: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblMiners: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    Shape5: TShape;
    procedure ResizeToFit;
  private

  public
    MainForm: TForm;
  end;

var
  StatisticsForm: TStatisticsForm;

implementation

{$R *.lfm}

{ TStatisticsForm }

procedure TStatisticsForm.ResizeToFit;
begin
  if Assigned(MainForm) then
  begin
    Left := MainForm.Left + MainForm.Width + 8;
    Top := MainForm.Top;
  end;
end;

end.

