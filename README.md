※ ごめんなさい、まだ執筆中です。(>_<)

# RaytracerCL

[OpenCL](https://ja.wikipedia.org/wiki/OpenCL) を用いて、ＧＰＵ上でレイトレーシングする方法。

![](https://github.com/LUXOPHIA/RaytracerCL/raw/Lesson-01/--------/_SCREENSHOT/RaytracerCL.png)

## ■ OpenCL について
基本的なライブセラリの使い方は、以下を参照して下さい。
* https://luxophia.github.io/OpenCL/ja/

## ■ Main.pas

### ▼ TForm1
基本的なレイトレーシングの実装において、中核をなすクラスは以下の６つ（★）である。

* `_Platfo :TCLPlatfo`
* `_Device :TCLDevice`
* `_Contex :TCLContex`
* `_Queuer :TCLQueuer`
* `_Imager :TCLImager2DxBGRAxUFix8` ★  
8bit 整数のＲＧＢＡ画像。
* `_ImaFMX :ICLStream2DxBGRAxUFix8_FMX` ★  
`_Imager`を`FMX.Graphics.TBitmap`として扱うためのクラス。
* `_Camera :TCLBuffer<TSingleM4>` ★  
カメラの座標変換行列。
* `_Textur :TCLImager2DxRGBAxSFlo32` ★  
32bit 実数のＲＧＢＡ画像。
* `_TexHDR :ICLStream2DxRGBAxSFlo32_HDR` ★  
`_Textur`を`*.hdr`画像として扱うためのクラス。
* `_Samplr :TCLSamplr` ★  
`_Textur`の補間方法を指定するクラス。
* `_Execut :TCLExecut`
* `_Buildr :TCLBuildr`
* `_Kernel :TCLKernel`

### ▼ TForm1.FormCreate
アプリ起動時に３つの初期化（★）を行う。
```delphi
procedure TForm1.FormCreate(Sender: TObject);
begin
     _MouseS := [];
     _MouseC := TPointF.Create( -60, +10 );
     MakeContext; ★
     MakeArgumes; ★
     MakeProgras; ★
     ShowBuild;
     TOpenCL.Show( MemoS.Lines );
end;
```

### ▼ TForm1.MakeContext
OpenCL の初期化を行う。
```delphi
procedure TForm1.MakeContext;
begin
     _Platfo := TOpenCL.Platfos[ 0 ];
     _Device := _Platfo.Devices[ 0 ];
     _Contex := TCLContex.Create( _Platfo );
     _Queuer := _Contex.Queuers[ _Device ];
end;
```

### ▼ TForm1.MakeArgumes
OpenCL とデータをやり取りするための各種クラスを生成し設定する。
```delphi
procedure TForm1.MakeArgumes;
begin
     _Imager := TCLImager2DxBGRAxUFix8.Create( _Contex, _Queuer );
     _Imager.CountX := 800;
     _Imager.CountY := 600;

     _ImaFMX := TCLStream2DxBGRAxUFix8_FMX.Create( _Imager );

     _Camera := TCLBuffer<TSingleM4>.Create( _Contex, _Queuer );
     _Camera.Count := 1;

     _Textur := TCLImager2DxRGBAxSFlo32.Create( _Contex, _Queuer );

     _TexHDR := TCLStream2DxRGBAxSFlo32_HDR.Create( _Textur );
     _TexHDR.LoadFromFile( '..\..\_DATA\Luxo-Jr_2000x1000.hdr' );

     _Samplr := TCLSamplr.Create( _Contex );
end;
```

### ▼ TForm1.MakeProgras
OpenCL のコードをロードしてコンパイルする。
```delphi
procedure TForm1.MakeProgras;
begin
     with _Contex.Librars do
     begin
          Add.Source.LoadFromFile( '..\..\_DATA\Math.cl'              );
          Add.Source.LoadFromFile( '..\..\_DATA\Math.D4x4.cl'         );
          Add.Source.LoadFromFile( '..\..\_DATA\Color.cl'             );
          Add.Source.LoadFromFile( '..\..\_DATA\Raytracing.cl'        );
          Add.Source.LoadFromFile( '..\..\_DATA\Raytrace.Object.cl'   );
          Add.Source.LoadFromFile( '..\..\_DATA\Raytrace.Material.cl' );
     end;

     _Execut := TCLExecut.Create( _Contex );
     _Execut.Source.LoadFromFile( '..\..\_DATA\Raytracing.Render.cl' );

     _Buildr := _Execut.Buildrs[ _Device ];

     if not Assigned( _Buildr.Handle ) then Exit; { _Buildr is Error! }

     _Kernel := _Execut.Kernels.Add( 'Render', _Queuer );

     Assert( Assigned( _Kernel.Handle ), '_Kernel is Error!' );

     _Kernel.GloSizX := _Imager.CountX;
     _Kernel.GloSizY := _Imager.CountY;

     _Kernel.Parames['Imager'] := _Imager;
     _Kernel.Parames['Camera'] := _Camera;
     _Kernel.Parames['Textur'] := _Textur;
     _Kernel.Parames['Samplr'] := _Samplr;

     Assert( _Kernel.Parames.FindsOK, '_Kernel.Parames.FindsOK is Error!' );
     Assert( _Kernel.Parames.BindsOK, '_Kernel.Parames.BindsOK is Error!' );

     Timer1.Enabled := True;
end;
```

----

* **Delphi IDE** @ Embarcadero  
https://www.embarcadero.com/jp/products/delphi/starter
