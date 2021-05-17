unit Main;

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Objects, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.TabControl,
  cl_version, cl_platform, cl,
  LUX, LUX.D1, LUX.D2, LUX.D3, LUX.D4, LUX.D4x4,
  LUX.GPU.OpenCL,
  LUX.GPU.OpenCL.FMX;

type
  TForm1 = class(TForm)
    TabControl1: TTabControl;
      TabItemS: TTabItem;
        MemoS: TMemo;
      TabItemP: TTabItem;
            MemoPB: TMemo;
      TabItemR: TTabItem;
        Image1: TImage;
    Timer1: TTimer;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  private
    { private 宣言 }
    _MouseS :TShiftState;
    _MouseP :TPointF;
    _MouseC :TPointF;
  public
    { public 宣言 }
    _Platfo :TCLPlatfo;
    _Device :TCLDevice;
    _Contex :TCLContex;
    _Queuer :TCLQueuer;
    _Buffer :TCLDevBuf<TSingleM4>;
    _Imager :TCLDevIma2DxBGRAxUFix8;
    _Textur :TCLDevIma2DxRGBAxSFlo32;
    _Samplr :TCLSamplr;
    _Execut :TCLExecut;
    _Buildr :TCLBuildr;
    _Kernel :TCLKernel;
    ///// メソッド
    procedure ShowBuildr;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

uses System.Math;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

procedure TForm1.ShowBuildr;
begin
     with MemoPB.Lines do
     begin
          if _Buildr.CompileStatus = CL_BUILD_ERROR then
          begin
               Add( '▼ Compile' );
               Add( _Buildr.CompileLog );
               Add( '' );
          end;

          if _Buildr.LinkStatus = CL_BUILD_ERROR then
          begin
               Add( '▼ Link' );
               Add( _Buildr.LinkLog );
               Add( '' );
          end;
     end;

     TabControl1.ActiveTab := TabItemP;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

procedure TForm1.FormCreate(Sender: TObject);
begin
     _MouseS := [];
     _MouseC := TPointF.Create( 0, 0 );

     _Platfo := TOpenCL.Platfos[ 0 ];
     _Device := _Platfo.Devices[ 0 ];
     _Contex := _Platfo.Contexs.Add;
     _Queuer := _Contex.Queuers.Add( _Device );

     _Buffer := TCLDevBuf<TSingleM4>.Create( _Contex, _Queuer );
     _Buffer.Count := 1;

     _Imager := TCLDevIma2DxBGRAxUFix8.Create( _Contex, _Queuer );
     _Imager.CountX := 800;
     _Imager.CountY := 600;

     _Textur := TCLDevIma2DxRGBAxSFlo32.Create( _Contex, _Queuer );
     _Textur.LoadFromFileHDR( '..\..\_DATA\Luxo-Jr_2000x1000.hdr' );

     _Samplr := TCLSamplr.Create( _Contex );

     with _Contex.Librars do
     begin
          Add.Source.LoadFromFile( '..\..\_DATA\Math.cl'              );
          Add.Source.LoadFromFile( '..\..\_DATA\Math.D4x4.cl'         );
          Add.Source.LoadFromFile( '..\..\_DATA\Color.cl'             );
          Add.Source.LoadFromFile( '..\..\_DATA\Raytrace.core.cl'     );
          Add.Source.LoadFromFile( '..\..\_DATA\Raytrace.Object.cl'   );
          Add.Source.LoadFromFile( '..\..\_DATA\Raytrace.Material.cl' );
     end;

     _Execut := _Contex.Executs.Add;
     _Execut.Source.LoadFromFile( '..\..\_DATA\Raytrace.cl' );

     _Buildr := _Execut.BuildTo( _Device );

     if Assigned( _Buildr.Handle ) then
     begin
          _Kernel := _Execut.Kernels.Add( 'Main', _Queuer );
          _Kernel.Parames['Buffer'] := _Buffer;
          _Kernel.Parames['Imager'] := _Imager;
          _Kernel.Parames['Textur'] := _Textur;
          _Kernel.Parames['Samplr'] := _Samplr;
          _Kernel.GloSizX := _Imager.CountX;
          _Kernel.GloSizY := _Imager.CountY;

          if _Kernel.Parames.BindsOK then Timer1.Enabled := True
                                     else TabControl1.ActiveTab := TabItemS;
     end
     else ShowBuildr;

     TOpenCL.Show( MemoS.Lines );
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     MemoS.Lines.SaveToFile( 'System.txt', TEncoding.UTF8 );

     Image1.Bitmap.SaveToFile( 'Imager.png' )
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.Timer1Timer(Sender: TObject);
begin
     _Buffer.Storag.Map;
     _Buffer.Storag[ 0 ] := TSingleM4.RotateY( DegToRad( -_MouseC.X ) )
                          * TSingleM4.RotateX( DegToRad( -_MouseC.Y ) )
                          * TSingleM4.Translate( 0, 0, 3 );
     _Buffer.Storag.Unmap;

     _Kernel.Run;

     _Imager.CopyTo( Image1.Bitmap );
end;

//------------------------------------------------------------------------------

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
     _MouseS := Shift;
     _MouseP := TPointF.Create( X, Y );
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
   P :TPointF;
begin
     if ssLeft in _MouseS then
     begin
          P := TPointF.Create( X, Y );
          _MouseC := _MouseC + ( P - _MouseP );
          _MouseP := P;
     end;
end;

procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
     Image1MouseMove( Sender, Shift, X, Y );

     _MouseS := [];
end;

end. //######################################################################### ■
