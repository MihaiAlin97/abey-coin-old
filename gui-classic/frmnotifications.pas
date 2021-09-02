unit frmNotifications;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  TAGraph, TASeries;

type

  { TNotificationsForm }

  TNotificationsForm = class(TForm)
    ReceivedLineSeries: TLineSeries;
    SentLineSeries: TLineSeries;
    BandwidthChart: TChart;
    Label9: TLabel;
    ReceivedLineSeries1: TLineSeries;
    SentLineSeries1: TLineSeries;
    NetworkChart: TChart;
    imgNetworkOn: TImage;
    imgNetworkOff: TImage;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lblClients: TLabel;
    lblServers: TLabel;
    lblSent: TLabel;
    lblReceived: TLabel;
    lblMessages: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    procedure imgNetworkOnClick(Sender: TObject);
    procedure ResizeToFit;
  private

  public
    MainForm: TForm;
  end;

var
  NotificationsForm: TNotificationsForm;

implementation

{$R *.lfm}

uses UFRMWallet;

procedure TNotificationsForm.ResizeToFit;
begin
  if Assigned(MainForm) then
  begin
    Left := MainForm.Left + MainForm.Width + 8;
    Top := MainForm.Top + (MainForm as TFRMWallet).frmStatistics.Height;
  end;
end;

procedure TNotificationsForm.imgNetworkOnClick(Sender: TObject);
begin

end;

end.

