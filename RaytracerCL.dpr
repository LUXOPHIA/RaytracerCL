﻿program RaytracerCL;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  LUX.Code.C in '_LIBRARY\LUXOPHIA\LUX\Code\LUX.Code.C.pas',
  LUX in '_LIBRARY\LUXOPHIA\LUX\LUX.pas',
  cl_platform in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\CL\cl_platform.pas',
  cl_version in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\CL\cl_version.pas',
  cl in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\CL\cl.pas',
  LUX.Data.List.core in '_LIBRARY\LUXOPHIA\LUX\Data\List\LUX.Data.List.core.pas',
  LUX.Data.List in '_LIBRARY\LUXOPHIA\LUX\Data\List\LUX.Data.List.pas',
  LUX.D1 in '_LIBRARY\LUXOPHIA\LUX\LUX.D1.pas',
  LUX.Color.Format.HDR in '_LIBRARY\LUXOPHIA\LUX\Color\LUX.Color.Format.HDR.pas',
  LUX.Color.Grid.D1.Preset in '_LIBRARY\LUXOPHIA\LUX\Color\_FMX\LUX.Color.Grid.D1.Preset.pas',
  LUX.Color.Grid.D2 in '_LIBRARY\LUXOPHIA\LUX\Color\_FMX\LUX.Color.Grid.D2.pas',
  LUX.Color.Grid.D2.Preset in '_LIBRARY\LUXOPHIA\LUX\Color\_FMX\LUX.Color.Grid.D2.Preset.pas',
  LUX.Color.Grid.D3 in '_LIBRARY\LUXOPHIA\LUX\Color\_FMX\LUX.Color.Grid.D3.pas',
  LUX.Color.Grid.D1 in '_LIBRARY\LUXOPHIA\LUX\Color\_FMX\LUX.Color.Grid.D1.pas',
  LUX.Color in '_LIBRARY\LUXOPHIA\LUX\Color\LUX.Color.pas',
  LUX.Data.Grid.T1.D1 in '_LIBRARY\LUXOPHIA\LUX\Data\Grid\LUX.Data.Grid.T1.D1.pas',
  LUX.Data.Grid.T1 in '_LIBRARY\LUXOPHIA\LUX\Data\Grid\LUX.Data.Grid.T1.pas',
  LUX.Data.Grid.T2.D1 in '_LIBRARY\LUXOPHIA\LUX\Data\Grid\LUX.Data.Grid.T2.D1.pas',
  LUX.Data.Grid.T2 in '_LIBRARY\LUXOPHIA\LUX\Data\Grid\LUX.Data.Grid.T2.pas',
  LUX.Data.Grid.T3.D3 in '_LIBRARY\LUXOPHIA\LUX\Data\Grid\LUX.Data.Grid.T3.D3.pas',
  LUX.Data.Grid.T3 in '_LIBRARY\LUXOPHIA\LUX\Data\Grid\LUX.Data.Grid.T3.pas',
  LUX.Data.Grid in '_LIBRARY\LUXOPHIA\LUX\Data\Grid\LUX.Data.Grid.pas',
  LUX.D2 in '_LIBRARY\LUXOPHIA\LUX\LUX.D2.pas',
  LUX.D3 in '_LIBRARY\LUXOPHIA\LUX\LUX.D3.pas',
  LUX.D4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D4.pas',
  LUX.D4x4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D4x4.pas',
  LUX.Curve.CatRom in '_LIBRARY\LUXOPHIA\LUX\Curve\LUX.Curve.CatRom.pas',
  LUX.Curve in '_LIBRARY\LUXOPHIA\LUX\Curve\LUX.Curve.pas',
  LUX.D2x2 in '_LIBRARY\LUXOPHIA\LUX\LUX.D2x2.pas',
  LUX.D3x3 in '_LIBRARY\LUXOPHIA\LUX\LUX.D3x3.pas',
  LUX.Curve.BSpline in '_LIBRARY\LUXOPHIA\LUX\Curve\LUX.Curve.BSpline.pas',
  LUX.Curve.BSpline.D2 in '_LIBRARY\LUXOPHIA\LUX\Curve\LUX.Curve.BSpline.D2.pas',
  LUX.GPU.OpenCL.Argume.Memory.Buffer in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Core\LUX.GPU.OpenCL.Argume.Memory.Buffer.pas',
  LUX.GPU.OpenCL.Argume.Memory.Imager.D1 in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Core\LUX.GPU.OpenCL.Argume.Memory.Imager.D1.pas',
  LUX.GPU.OpenCL.Argume.Memory.Imager.D2 in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Core\LUX.GPU.OpenCL.Argume.Memory.Imager.D2.pas',
  LUX.GPU.OpenCL.Argume.Memory.Imager.D3 in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Core\LUX.GPU.OpenCL.Argume.Memory.Imager.D3.pas',
  LUX.GPU.OpenCL.Argume.Memory.Imager in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Core\LUX.GPU.OpenCL.Argume.Memory.Imager.pas',
  LUX.GPU.OpenCL.Argume.Memory in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Core\LUX.GPU.OpenCL.Argume.Memory.pas',
  LUX.GPU.OpenCL.Argume in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Core\LUX.GPU.OpenCL.Argume.pas',
  LUX.GPU.OpenCL.Argume.Samplr in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Core\LUX.GPU.OpenCL.Argume.Samplr.pas',
  LUX.GPU.OpenCL.Contex in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Core\LUX.GPU.OpenCL.Contex.pas',
  LUX.GPU.OpenCL.core in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Core\LUX.GPU.OpenCL.core.pas',
  LUX.GPU.OpenCL.Device in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Core\LUX.GPU.OpenCL.Device.pas',
  LUX.GPU.OpenCL.Kernel in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Core\LUX.GPU.OpenCL.Kernel.pas',
  LUX.GPU.OpenCL.Platfo in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Core\LUX.GPU.OpenCL.Platfo.pas',
  LUX.GPU.OpenCL.Progra in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Core\LUX.GPU.OpenCL.Progra.pas',
  LUX.GPU.OpenCL.Queuer in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Core\LUX.GPU.OpenCL.Queuer.pas',
  LUX.GPU.OpenCL.Show in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Core\LUX.GPU.OpenCL.Show.pas',
  LUX.GPU.OpenCL in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\LUX.GPU.OpenCL.pas',
  LUX.GPU.OpenCL.Argume.Seeder.D3 in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Argume\LUX.GPU.OpenCL.Argume.Seeder.D3.pas',
  LUX.GPU.OpenCL.Argume.Seeder in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Argume\LUX.GPU.OpenCL.Argume.Seeder.pas',
  LUX.GPU.OpenCL.Argume.Seeder.D1 in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Argume\LUX.GPU.OpenCL.Argume.Seeder.D1.pas',
  LUX.GPU.OpenCL.Argume.Seeder.D2 in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Argume\LUX.GPU.OpenCL.Argume.Seeder.D2.pas',
  LUX.GPU.OpenCL.Stream.FMX.D2 in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Stream\LUX.GPU.OpenCL.Stream.FMX.D2.pas',
  LUX.GPU.OpenCL.Stream.FMX.D3 in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Stream\LUX.GPU.OpenCL.Stream.FMX.D3.pas',
  LUX.GPU.OpenCL.Stream.HDR.D1 in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Stream\LUX.GPU.OpenCL.Stream.HDR.D1.pas',
  LUX.GPU.OpenCL.Stream.HDR.D2 in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Stream\LUX.GPU.OpenCL.Stream.HDR.D2.pas',
  LUX.GPU.OpenCL.Stream.HDR.D3 in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Stream\LUX.GPU.OpenCL.Stream.HDR.D3.pas',
  LUX.GPU.OpenCL.Stream.FMX.D1 in '_LIBRARY\LUXOPHIA\LUX.GPU.OpenCL\Stream\LUX.GPU.OpenCL.Stream.FMX.D1.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

